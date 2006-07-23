
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General
import Data.Maybe


type Env = (Hite, DataM SmallT, Func2M)
type Stack = [((FuncName, [AExp]), AExp)]


data AExp = Value Abstract
          | ASel AExp CtorArg
          | AMake CtorName [AExp]
          | AFunc FuncName
          | ACall AExp [AExp]
          | Union [AExp]
          | ACase AExp [(CtorName, AExp)]
          deriving (Show,Eq)


unionAExp :: [AExp] -> AExp
unionAExp [] = Value AbsVoid
unionAExp xs = foldr1 f xs
    where
        f (Union a) (Union b) = Union (a++b)
        f (Union a) b = Union (b:a)
        f a (Union b) = Union (a:b)
        f (Value a) (Value b) = Value $ unionAbs [a,b]
        f x y = Union [x,y]



fromValue (Value x) = x


evaluate :: (String -> IO ()) -> Hite -> DataM SmallT -> Func2M -> [Abstract] -> IO Abstract
evaluate logger hite datam funcm args = do
    Value res <- evalCall logger (hite, datam, funcm) [] "main" (map Value args)
    return res


permuteAExp :: AExp -> [AExp]
permuteAExp (Value x) = map Value $ permuteAbs x
permuteAExp x = [x]


evalCall :: (String -> IO ()) -> Env -> Stack -> FuncName -> [AExp] -> IO AExp
evalCall logger env@(hite,datam,funcm) stack func args
        | isJust prev = return $ fromJust prev
        | length args2 == 1 = f 0 AbsVoid >>= return . Value
        | otherwise = g 0 AbsVoid >>= return . Value
    where
        args2 = crossProduct $ map permuteAExp args
        pad = replicate (length stack * 2) ' '
        
        g n x = do
            logger $ pad ++ func ++ "*" ++ show n ++ " " ++ show args ++ " = " ++ show x
            res <- mapM (evalCall logger env (((func,args),Value x):stack) func) args2
            let res2 = unionAbs (x:map fromValue res)
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else g (n+1) res2
    
        f n x = do
            logger $ pad ++ func ++ ":" ++ show n ++ " " ++ show args ++ " = " ++ show x
            res <- evalExpr logger env (((func,args),Value x):stack) abody
            let res1 = case res of
                           Value x -> x
                           _ -> error $ "evalCall " ++ func ++ " " ++ show args ++ " = " ++ show res
            let res2 = unionAbs [res1,x]
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else f (n+1) res2
    
        abody = exprToAExp (zip arg args) body
        Func _ arg body _ = getFunc hite func
        prev = lookup (func,args) stack


evalExpr :: (String -> IO ()) -> Env -> Stack -> AExp -> IO AExp
evalExpr logger env@(hite,datam,funcm) stack x =
    case x of
        ACall (AFunc name) args -> do
            args2 <- mapM f args
            let largs = length $ funcArgs $ getFunc hite name
            if largs == length args then
                evalCall logger env stack name args2
             else if null args then
                return $ AFunc name
             else
                return $ x
                
        ACall (ACall x xs) ys -> f (ACall x (xs ++ ys))
    
        AFunc x -> return $ AFunc x
    
        Value x -> return $ Value x
        
        ACase x alts -> do
            Value x2 <- f x
            alts2 <- mapM (g x2) alts
            return $ unionAExp $ concat alts2
            where
                g x2 (opt,expr) = if hasCtorAbs datam x2 opt
                                     then f expr >>= return . (:[])
                                     else return []
    
        ASel x y -> do
            Value x2 <- f x
            return $ Value $ followSelAbs hite datam x2 y
    
        AMake name xs -> do
            xs2 <- mapM f xs
            return $ Value $ makeAbs datam name (map fromValue xs2)
    
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
