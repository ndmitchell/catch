
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General
import Data.Maybe


type AbstractA = Abstract AExp


type Env = (Hite, DataM SmallT, Func2M)
type Stack = [((FuncName, [AbstractA]), AbstractA)]


data AExp = Value AbstractA
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


evaluate :: (String -> IO ()) -> Hite -> DataM SmallT -> Func2M -> [Abstract ()] -> IO (Abstract ())
evaluate logger hite datam funcm args = do
    res <- evalCall logger (hite, datam, funcm) [] "main" (map liftAbs args)
    return $ liftAbs res


permuteAExp :: AExp -> [AExp]
permuteAExp (Value x) = map Value $ permuteAbs x
permuteAExp x = [x]


evalCall :: (String -> IO ()) -> Env -> Stack -> FuncName -> [AbstractA] -> IO AbstractA
evalCall logger env@(hite,datam,funcm) stack func args
        | isJust prev = return $ fromJust prev
        | length args2 == 1 = f 0 AbsVoid
        | otherwise = g 0 AbsVoid
    where
        args2 = crossProduct $ map permuteAbs args
        pad = replicate (length stack * 2) ' '
        
        g n x = do
            logger $ pad ++ func ++ "*" ++ show n ++ " " ++ show args ++ " = " ++ show x
            res <- mapM (evalCall logger env (((func,args),x):stack) func) args2
            let res2 = unionAbs (x:res)
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else g (n+1) res2
    
        f n x = do
            logger $ pad ++ func ++ ":" ++ show n ++ " " ++ show args ++ " = " ++ show x
            res <- evalExpr logger env (((func,args),x):stack) abody
            let res2 = unionAbs (x:res:[])
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else f (n+1) res2
    
        abody = exprToAExp (zip arg args) body
        Func _ arg body _ = getFunc hite func
        prev = lookup (func,args) stack


evalExpr :: (String -> IO ()) -> Env -> Stack -> AExp -> IO AbstractA
evalExpr logger env@(hite,datam,funcm) stack x =
    case x of
        ACall (AFunc name) args -> do
            args2 <- mapM f args
            let largs = length $ funcArgs $ getFunc hite name
            if largs == length args then
                evalCall logger env stack name args2
             else if null args then
                return $ AbsOther [AFunc name]
             else
                return $ AbsOther [x]
                
        ACall (ACall x xs) ys -> f (ACall x (xs ++ ys))
    
        AFunc x -> return $ AbsOther [AFunc x]
    
        Value x -> return $ x
        
        ACase x alts -> do
            x2 <- f x
            alts2 <- mapM (g x2) alts
            return $ unionAbs $ [AbsBottom | headBottom x2] ++ concat alts2
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


exprToAExp :: [(String, AbstractA)] -> Expr -> AExp
exprToAExp args x =
    case x of
        Call a as -> ACall (f a) (map f as)
        CallFunc a -> AFunc a
        Sel a b -> ASel (f a) b
        Error _ -> Value AbsBottom
        Case a as -> ACase (f a) [(a,f b) | (a,b) <- as]
        Var a -> addValue $ lookupJust a args
        Make a as -> AMake a (map f as)
        _ -> error $ "exprToAExp, todo: " ++ show x

    where
        f x = exprToAExp args x


addValue :: AbstractA -> AExp
addValue (AbsOther [x]) = x
addValue x = Value x



eval :: Env -> [(String, AbstractA )] -> Expr -> AbstractA
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
