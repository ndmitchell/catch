
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General
import Data.Maybe
import Typey.Faster


type AbstractA = Abstract AExp


type Env = (Hite, DataM SmallT, FunctionM)
type Stack = [((FuncName, [AbstractA]), AbstractA)]



type FunctionM = [(FuncName,Function)]

data Function =
    Function {
        funArgLen :: Int,
        funArgs :: [String],
        funFixpoint :: Bool, -- do I need to fixed point
        funArgsPower :: [Bool], -- do I need to take the power of these arguments
        funBody :: Expr
        }


generateFunctions :: Hite -> FunctionM
generateFunctions hite = map f (funcs hite)
    where
        f (Func name args body _) = (name, Function largs args True (replicate largs True) body)
            where largs = length args



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
    res <- evalCall logger (hite, datam, generateFunctions hite) [] "main" (map liftAbs args)
    return $ liftAbs res


permuteAExp :: AExp -> [AExp]
permuteAExp (Value x) = map Value $ permuteAbs x
permuteAExp x = [x]


evalCall :: (String -> IO ()) -> Env -> Stack -> FuncName -> [AbstractA] -> IO AbstractA
evalCall logger env@(hite,datam,funcm) stack func args
        | isJust prev = return $ fromJust prev
        | canFastEval func = solveFast
        | length args2 == 1 = f 0 AbsVoid
        | otherwise = g 0 AbsVoid
    where
        solveFast = do
            logger $ msg 0 ++ "(fast)"
            res <- fastEval doEval func args
            logger $ pad ++ "= " ++ show res
            return res
    
        -- the fast eval helper
        doEval :: [AbstractA] -> IO AbstractA
        doEval (x:xs) = evalExpr logger env (((func,args),AbsVoid):stack) (ACall (addValue x) (map addValue xs))
    
        args2 = crossProduct $ map permuteAbs args
        pad = replicate (length stack * 2) ' '
        
        msg n = pad ++ func ++ ":" ++ show n ++ " " ++ show args ++ " = "
        
        g n x = do
            logger $ msg n ++ show x
            res <- mapM (evalCall logger env (((func,args),x):stack) func) args2
            let res2 = unionAbs (x:res)
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else g (n+1) res2
    
        f n x = do
            logger $ msg n ++ show x
            res <- evalExpr logger env (((func,args),x):stack) abody
            let res2 = unionAbs (x:res:[])
            if res2 == x
                then logger (pad ++ "= " ++ show x) >> return x
                else f (n+1) res2
    
        abody = exprToAExp (zip (funArgs fun) args) (funBody fun)
        fun = lookupJust func funcm
        prev = lookup (func,args) stack


evalExpr :: (String -> IO ()) -> Env -> Stack -> AExp -> IO AbstractA
evalExpr logger env@(hite,datam,funcm) stack x =
    case x of
        ACall (AFunc name) args -> do
            args2 <- mapM f args
            let largs = funArgLen $ lookupJust name funcm
                (argsNow, argsLater) = splitAt largs args2
            
            if length argsNow == largs then do
                res <- evalCall logger env stack name argsNow
                if null argsLater
                    then return res
                    else f (ACall (addValue res) $ map addValue argsLater)
             else if null args then
                return $ AbsOther [AFunc name]
             else
                return $ AbsOther [ACall (AFunc name) $ map addValue args2]
                
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
        Call (CallFunc name) params -> eval env (zip (funArgs func) args2) (funBody func)
            where
                func = lookupJust name funcm
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
