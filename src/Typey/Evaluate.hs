
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General
import Data.Maybe


type Env = (Hite, DataM SmallT, Func2M)
type Stack = [((FuncName, [AExp]), Abstract)]


data AExp = Value Abstract
          | ASel AExp CtorArg
          | AMake CtorName [AExp]
          | AFunc FuncName
          | ACall AExp [AExp]
          | Union [AExp]
          | ACase AExp [(CtorName, AExp)]
          deriving (Show,Eq)


fromValue (Value x) = x


evaluate :: (String -> IO ()) -> Hite -> DataM SmallT -> Func2M -> [Abstract] -> IO Abstract
evaluate logger hite datam funcm args = evalCall logger (hite, datam, funcm) [] "main" (map Value args)


evalCall :: (String -> IO ()) -> Env -> Stack -> FuncName -> [AExp] -> IO Abstract
evalCall logger env@(hite,datam,funcm) stack func args
        | isJust prev = return $ fromJust prev
        | otherwise = f AbsVoid
    where
        f x = do res <- evalExpr logger env (((func,args),x):stack) abody
                 let res2 = unionAbs [res,x]
                 if res2 == x
                    then return x
                    else f res2
    
        abody = exprToAExp (zip arg args) body
        Func _ arg body _ = getFunc hite func
        prev = lookup (func,args) stack


evalExpr :: (String -> IO ()) -> Env -> Stack -> AExp -> IO Abstract
evalExpr logger env@(hite,datam,funcm) stack x =
    case x of
        ACall (AFunc name) args -> do
            args2 <- mapM f args
            evalCall logger env stack name (map Value args2)
    
        Value x -> return x
        
        ACase x alts -> do
            x2 <- f x
            alts2 <- mapM (g x2) alts
            return $ unionAbs $ concat alts2
            where
                g x2 (opt,expr) = if hasCtorAbs datam x2 opt
                                     then f expr >>= return . (:[])
                                     else return []
    
        ASel x y -> do
            x2 <- f x
            return $ followSelAbs hite datam x2 y
    
        AMake name xs -> do
            xs2 <- mapM f xs
            return $ makeAbs datam name xs2
    
        _ -> error $ "evalExpr, todo: " ++ show x

    where
        f x = evalExpr logger env stack x


exprToAExp :: [(String, AExp)] -> Expr -> AExp
exprToAExp args x =
    case x of
        Call a as -> ACall (f a) (map f as)
        CallFunc a -> AFunc a
        Sel a b -> ASel (f a) b
        Error _ -> Value AbsBottom
        Case a as -> ACase (f a) [(a,f b) | (a,b) <- as]
        Var a -> lookupJust a args
        Make a as -> AMake a (map f as)
        _ -> error $ "exprToAExp, todo: " ++ show x

    where
        f x = exprToAExp args x



eval :: Env -> [(String, Abstract)] -> Expr -> Abstract
eval env@(hite,datam,funcm) args expr =
    case expr of
        Call (CallFunc name) params -> eval env (zip arg args2) body
            where
                Func _ arg body _ = getFunc hite name
                args2 = map (eval env args) params
        
        Case x alts -> unionAbs $ concatMap f alts
            where
                x2 = eval env args x
                f (opt,expr) = if hasCtorAbs datam x2 opt then [eval env args expr] else []
        
        Var x -> lookupJust x args
        
        Sel x y -> followSelAbs hite datam (eval env args x) y
        
        Make name xs -> makeAbs datam name (map (eval env args) xs)
        
        Error _ -> AbsBottom
        
        x -> error $ "eval: " ++ output x
