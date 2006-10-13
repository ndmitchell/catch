
module Hill.Specialise(cmdsSpecialise) where

import Hill.Type
import Hill.Lambdas
import qualified Data.Map as Map
import Control.Monad.State


cmdsSpecialise = [hillCmdPure "specialise" (const specialise)]


---------------------------------------------------------------------

specialise :: Hill -> Hill
specialise hill = error $ "\n" ++ showTemplate (generate hill)




-- function name, arguments, result (if known)
-- use Var 0 to represent unknown information
-- equivalent to ? in this context
type Template = Map.Map (FuncName, [Expr]) (Maybe Expr)


showTemplate :: Template -> String
showTemplate xs = unlines $ map f $ Map.toList xs
    where
        f ((name,args),res) = show (Call name args) ++ " = " ++ g res
        
        g Nothing = "<NOTHING>"
        g (Just x) = show x



generate :: Hill -> Template
generate hill = execState (evalFunc "main" mainArgs) Map.empty
    where
        mainArgs = replicate (length $ funcArgs $ getFunc hill "main") (Var 0)
    
        -- find an item in the template
        evalFunc :: FuncName -> [Expr] -> State Template Expr
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

        
        evalExpr :: Expr -> State Template Expr
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
        
