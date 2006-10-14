
module Hill.Specialise(cmdsSpecialise) where

import Hill.Type
import Hill.Lambdas
import Hill.Show

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import General.General
import Front.CmdLine
import System.IO


cmdsSpecialise = [Action "hill-specialise" specialise]


---------------------------------------------------------------------

specialise :: CmdLineState -> String -> ValueHill -> IO ValueHill
specialise state _ (ValueHill hillBad) = do
        hPutStrLn (cmdLineHandle state) $ showTemplate template
        return $ ValueHill $ makeCode hill template
    where
        hill = addLambdas hillBad
        template = filterTemplate $ makeTemplate hill


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
                body2 = mapOverHill makeExpr $ moveLambdas $ replaceFree (zip fargs newargs) body
                Func _ fargs body = getFunc hill name
                
                newname = fst $ Map.findWithDefault (name, Var 0) (name,args) template2
                (nargs, newargs) = ascendingFrees args
                
        makeExpr orig@(Apply (Fun x) xs) | arity <= length xs =
                case Map.lookup (x, args) template2 of
                    Nothing -> orig
                    Just (newname,_) -> Apply (Fun newname) (selArgs args as ++ bs)
            where
                args = map valueExpr as
                (as, bs) = splitAt arity xs
                arity = length $ funcArgs $ getFunc hill x
        
        makeExpr x = x
    
        selArgs args xs = concat $ zipWith selArg args xs
        
        selArg (Var 0) x = [x]
        selArg (Make x xs) (Apply (Const (ACtor y)) ys) = selArgs xs ys
        selArg arg x = selArgs (getChildren arg) (getChildren x)
        

        valueFunc func args = return $ Map.findWithDefault (Var 0) (func,args) template

        valueExpr x = runIdentity $ evalExpr hill valueFunc x


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
                    -- () <- if func == "risers" then error $ show $ replaceFree (zip funcArgs args) body else return ()
                    res <- evalExpr hill evalFunc body2
                    modify (Map.insert (func,args) (Just res))
                    return res


evalExpr :: Monad m => Hill -> (FuncName -> [Expr] -> m Expr) -> Expr -> m Expr
evalExpr hill evalFunc x = f x
    where
        f (Apply x xs) = do
            (y:ys) <- mapM f (x:xs)
            case y of
                Fun x | nxs >= nargs -> do
                        res <- evalFunc x as
                        f (mkApply res bs)
                    where
                        (as,bs) = splitAt nargs ys
                        nxs = length xs
                        nargs = length $ funcArgs $ getFunc hill x
                
                Const (ACtor x)
                    | length (ctorArgs $ getCtor hill x) == length ys
                    -> return $ Make x ys
                    
                _ -> return $ Var 0

        f (Fun x) | null (funcArgs func) = evalFunc x []
                  | otherwise = return $ Fun x
            where func = getFunc hill x

        f (Var _) = return $ Var 0
        f (Const x) = return $ Const x
        
        f (Lambda n (Fun x)) = return $ Lambda n (Fun x)
        f (Lambda n (Apply (Fun x) xs)) = liftM (Lambda n . Apply (Fun x)) $ mapM f xs
        
        f (Case on alts) = f on >> mapM_ (f . altExpr) alts >> return (Var 0)
        f (Prim x xs) = mapM_ f xs >> return (Var 0)
        
        f (Sel x y) = f x >> return (Var 0)
        
        f (Make x xs) = liftM (Make x) $ mapM f xs
        
        f (Let binds x) = mapM_ (f . snd) binds >> f x
        f (Error x) = f x >> return (Var 0)
        
        f x = error $ "Specialise.generate.evalExpr, unhandled " ++ show x

