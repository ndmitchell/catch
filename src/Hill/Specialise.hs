
module Hill.Specialise(cmdsSpecialise) where

import Hill.Type
import Hill.Lambdas
import qualified Data.Map as Map
import Control.Monad.State
import Data.Maybe
import General.General


cmdsSpecialise = [hillCmdPure "specialise" (const specialise)]


---------------------------------------------------------------------

specialise :: Hill -> Hill
specialise hill = makeCode hill template
    where template = filterTemplate $ makeTemplate hill


makeCode :: Hill -> Template Expr -> Hill
makeCode hill template = hill{funcs = map makeFunc needed}
    where
        needed = [(name, replicate (length args) (Var 0)) | Func name args _ <- funcs hill] ++ Map.keys template2
    
        template2 = snd $ Map.mapAccumWithKey genName 1 $ Map.filterWithKey checkUse template
            where
                genName n key val = (n+1, (fst key ++ "_" ++ show n, val))
                checkUse (key,args) val = any (not . isVar) args

        makeFunc (name, args) = Func newname [0..nargs-1] body2
            where
                body2 = makeExpr $ moveLambdas $ replaceFree (zip fargs newargs) body
                Func _ fargs body = getFunc hill name
                
                newname = fst $ Map.findWithDefault (name, Var 0) (name,args) template2
                (nargs, newargs) = ascendingFrees args

        makeExpr x = x


-- replace the free variables Var 0, with Var 0..Var n
ascendingFrees :: [Expr] -> (Int, [Expr])
ascendingFrees xs = fs xs 0
    where
        f :: Expr -> Int -> (Int, Expr)
        f (Var _) n = (n+1, Var n)
        f x n = (n2, setChildren x childs)
            where (n2, childs) = fs (getChildren x) n
        
        
        fs :: [Expr] -> Int -> (Int, [Expr])
        fs [] n = (n, [])
        fs (x:xs) n = (n3, x2:xs3)
            where
                (n2,x2) = f x n
                (n3,xs3) = fs xs n2



-- function name, arguments, result (if known)
-- use Var 0 to represent unknown information
-- equivalent to ? in this context
type Template a = Map.Map (FuncName, [Expr]) a


filterTemplate :: Template Expr -> Template Expr
filterTemplate template = Map.filterWithKey f template
    where f (name,args) res = any (not . isVar) (res:args)


showTemplate :: Show a => Template a -> String
showTemplate xs = unlines $ map f $ Map.toList xs
    where
        f ((name,args),res) = show (Call name args) ++ " = " ++ show res



makeTemplate :: Hill -> Template Expr
makeTemplate hill = Map.map fromJust $ execState (evalFunc "main" mainArgs) Map.empty
    where
        mainArgs = replicate (length $ funcArgs $ getFunc hill "main") (Var 0)
    
        -- find an item in the template
        evalFunc :: FuncName -> [Expr] -> State (Template (Maybe Expr)) Expr
        evalFunc func args = do
            s <- get
            case Map.lookup (func,args) s of
                Just x -> return $ case x of
                              Nothing -> Var 0
                              Just x -> x
                Nothing -> do
                    modify (Map.insert (func,args) Nothing)
                    let Func _ funcArgs body = getFunc hill func
                        body2 = moveLambdas $ replaceFree (zip funcArgs args) body
                    res <- evalExpr body2
                    modify (Map.insert (func,args) (Just res))
                    return res

        
        evalExpr :: Expr -> State (Template (Maybe Expr)) Expr
        evalExpr (Apply x xs) = do
            (y:ys) <- mapM evalExpr (x:xs)
            case y of
                Fun x | nxs >= nargs -> do
                        res <- evalFunc x as
                        evalExpr (mkApply res bs)
                    where
                        (as,bs) = splitAt nargs ys
                        nxs = length xs
                        nargs = length $ funcArgs $ getFunc hill x
                
                Const (ACtor x)
                    | length (ctorArgs $ getCtor hill x) == length ys
                    -> return $ Make x ys
                    
                _ -> return $ Var 0

        evalExpr (Var 0) = return $ Var 0
        evalExpr (Fun x) = return $ Fun x
        evalExpr (Const x) = return $ Const x
        
        evalExpr (Lambda n (Fun x)) = return $ Lambda n (Fun x)
        evalExpr (Lambda n (Apply (Fun x) xs)) = liftM (Lambda n . Apply (Fun x)) $ mapM evalExpr xs
        
        evalExpr (Case on alts) = evalExpr on >> mapM_ (evalExpr . altExpr) alts >> return (Var 0)
        evalExpr (Prim x xs) = mapM_ evalExpr xs >> return (Var 0)
        
        evalExpr (Sel x y) = evalExpr x >> return (Var 0)
        
        evalExpr (Make x xs) = liftM (Make x) $ mapM evalExpr xs
        
        evalExpr x = error $ "Specialise.generate.evalExpr, unhandled " ++ show x
