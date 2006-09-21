
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General
import Data.Maybe
import Data.List
import Typey.Faster
import Control.Monad

import Data.IORef
import qualified Data.Map as Map

type AbstractA = Abstract AExp


type Env = (Hite, DataM SmallT, FunctionM, Cache)
type Stack = [((FuncName, [AbstractA]), AbstractA)]

type Cache = IORef (Map.Map FuncName [([AbstractA], AbstractA)])


newCache :: IO Cache
newCache = newIORef Map.empty

getCache :: Cache -> FuncName -> [AbstractA] -> IO (Maybe AbstractA)
getCache cache func args = do
    c <- readIORef cache
    case Map.lookup func c of
        Nothing -> return Nothing
        Just x -> case lookup args x of
                      Nothing -> return Nothing
                      Just x -> do return () -- putStrLn $ "Cache hit, " ++ show (func,args,x)
                                   return $ Just x

addCache :: Cache -> FuncName -> [AbstractA] -> AbstractA -> IO ()
-- addCache cache func args res = return ()
addCache cache func args res = do
    -- putStrLn $ "Cache add, " ++ show (func,args,res)
    c <- readIORef cache
    let c2 = Map.insertWith (++) func [(args,res)] c
    writeIORef cache c2


dumpCache :: (String -> IO ()) -> Cache -> IO ()
dumpCache logger cache = do
    c <- readIORef cache
    logger $ show $ Map.toList c


type FunctionM = [(FuncName,Function)]

data Function =
    Function {
        funArgLen :: Int,
        funArgs :: [String],
        funCallset :: [FuncName], -- the functions i call
        funFixpoint :: Bool, -- do I need to fixed point
        funArgsPower :: [Bool], -- do I need to take the power of these arguments
        funBody :: Expr,
        funFast :: Bool
        }


generateFunctions :: Hite -> Func2M -> FunctionM
generateFunctions hite types = map f (funcs hite)
    where
        f (Func name args bod _) = (name,
                Function largs args callSet requiresFix (map f args) bod fast
                )
            where
                fast = canFastEval name
                largs = length args
                f x = (length [() | Var a <- allExpr bod, a == x] > 1) && (not fast)

                typ = lookupJust name types
                
                isHigher = case typ of
                               Arr2T xs res -> any isHigherOrder xs
                               x -> False
                
                requiresFix = isHigher || (name `elem` callSet)
                                
                callSet = fixSet g (g name)
                g name = [x | CallFunc x <- allExpr $ body $ getFunc hite name]
                

absFuncAExp :: AbstractA -> [FuncName]
absFuncAExp x = absFunc f x
    where
        f (Value x) = absFuncAExp x
        f (ASel x y) = f x
        f (AMake x y) = concatMap f y
        f (AFunc x) = [x]
        f (ACall x xs) = concatMap f (x:xs)
        f (Union xs) = concatMap f xs
        f (ACase x y) = concatMap f (x:map snd y)


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
    c <- newCache
    res <- evalCall logger (hite, datam, generateFunctions hite funcm, c) [] "main" (map liftAbs args)
    dumpCache logger c
    return $ liftAbs res


permuteAExp :: AExp -> [AExp]
permuteAExp (Value x) = map Value $ permuteAbs x
permuteAExp x = [x]


evalCall :: (String -> IO ()) -> Env -> Stack -> FuncName -> [AbstractA] -> IO AbstractA
evalCall logger env@(hite,datam,funcm,cache) stack func args
        | isJust prev = return $ fromJust prev
        | func == "_" = return AbsAny
        | funFast fun = solveFast
        | otherwise = solveSlow
    where
        cacheSafe = not $ any (`elem` stackSet) thisCallset
            where
                stackSet = map (fst . fst) stack
                thisCallset = nub $ funCallset fun ++ concatMap absFuncAExp args
    
        solveFast = do
            logger $ msg 0 ++ "(fast)"
            res <- fastEval doEval func args
            logger $ pad ++ "= " ++ show res
            return res
            
        solveSlow = if cacheSafe then solveCache else solveNoCache

        solveNoCache = if length args2 == 1 then f 0 AbsVoid else g 0 AbsVoid

        solveCache = do
            ca <- getCache cache func args
            case ca of
                Just r -> return r
                Nothing -> do
                    res <- solveNoCache
                    addCache cache func args res
                    return res

    
        -- the fast eval helper
        doEval :: [AbstractA] -> IO AbstractA
        doEval (x:xs) = evalExpr logger env (((func,args),AbsVoid):stack) (ACall (addValue x) (map addValue xs))
    
        args2 = crossProduct $ zipWithEq perm (funArgsPower fun) args
        perm True x = permuteAbs x
        perm False x = [x]
        pad = replicate (length stack * 2) ' '
        
        msg n = pad ++ func ++ ":" ++ show n ++ " " ++ show args ++ " = "
        
        g n x = do
            logger $ msg n ++ show x
            res <- mapM (evalCall logger env (((func,args),x):stack) func) args2
            let res2 = unionAbs (x:res)
            if norep || res2 == x
                then logger (pad ++ "= " ++ show res2) >> return res2
                else g (n+1) res2
    
        f n x = do
            logger $ msg n ++ show x
            res <- evalExpr logger env (((func,args),x):stack) abody
            let res2 = unionAbs (x:res:[])
            if norep || res2 == x
                then logger (pad ++ "= " ++ show res) >> return res2
                else f (n+1) res2
    
        abody = exprToAExp (zip (funArgs fun) args) (funBody fun)
        norep = not $ funFixpoint fun
        fun = lookupJust func funcm
        prev = lookup (func,args) stack


evalExpr :: (String -> IO ()) -> Env -> Stack -> AExp -> IO AbstractA
evalExpr logger env@(hite,datam,funcm,cache) stack x =
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
        _ -> error $ "exprToAExp, todo: " -- ++ show x

    where
        f x = exprToAExp args x


addValue :: AbstractA -> AExp
addValue (AbsOther [x]) = x
addValue x = Value x



eval :: Env -> [(String, AbstractA )] -> Expr -> AbstractA
eval env@(hite,datam,funcm,cache) args expr =
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
        
        x -> error $ "eval: " ++ show x
