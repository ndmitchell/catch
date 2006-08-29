
{-|
    A signature for the abstract type
-}

module Hite.Type(module Hite.Type, module Hite.DataType, module Hite.TypeType) where

import Hite.DataType
import Hite.TypeType

import General.General
import General.Commands
import Maybe
import List
import Char
import Data.Predicate


cmdHite :: (String -> Hite -> IO Hite) -> String -> String -> Command Hite
cmdHite f name desc = Command f name desc

cmdHitePure f = cmdHite (\a b -> return (f a b))

type FuncName = String
type FuncArg  = String

data Hite = Hite {datas :: [Data], funcs :: [Func]}
            deriving (Show, Read)

instance QDatas Hite where
	rawDatas = datas

data Func = Func {funcName :: FuncName, funcArgs :: [FuncArg], body :: Expr, pos :: String}
          deriving (Eq, Show, Read)

data Expr = Call {callFunc :: Expr, callArgs :: [Expr]}
          | Var {varArg :: FuncArg}
          | Sel {expr :: Expr, path :: CtorArg}
          | CallFunc {callName :: FuncName}
          | Make {makeName :: CtorName, makeArgs :: [Expr]}
          | Case Expr [(CtorName, Expr)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])
          | MCase [MCaseAlt]
          | Msg String
          
          -- constructor, list of arguments (nothing is recurse), expr is alt
          | Error String
          | Repeat {expr :: Expr, alt :: Expr}
          | RepeatNow
          | Bottom
          deriving (Eq, Show, Read)


data MCaseOpt = MCaseOpt Expr CtorName
                deriving (Eq, Show, Read)

data MCaseAlt = MCaseAlt (Pred MCaseOpt) Expr
                deriving (Eq, Show, Read)


instance PredLit MCaseOpt where
    a ?=> b = a == b
    (MCaseOpt e1 c1) ?/\ (MCaseOpt e2 c2) = if e1 == e2 && c1 /= c2 then Value False else Same


isVar (Var{}) = True; isVar _ = False
isSel (Sel{}) = True; isSel _ = False
isCallFunc (CallFunc{}) = True; isCallFunc _ = False


-- some basic utility functions
class PlayExpr a where
    mapExpr :: (Expr -> Expr) -> a -> a
    allExpr :: a -> [Expr]


instance PlayExpr a => PlayExpr [a] where
    mapExpr f xs = map (mapExpr f) xs
    allExpr xs = concatMap allExpr xs


instance PlayExpr Expr where
    mapExpr f x = f $ case x of
        Call a bs -> Call (mapExpr f a) (mapExpr f bs)
        Make a bs -> Make a             (mapExpr f bs)
        Case a bs -> Case (mapExpr f a) (map (\(d,e) -> (d,mapExpr f e)) bs)
        Sel  a b  -> Sel  (mapExpr f a) b
        MCase  as -> MCase (mapExpr f as)
        _ -> x
    
    allExpr x = x : concatMap allExpr (case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Case x xs -> x : map snd xs
            Sel  x _  -> [x]
            MCase  xs -> concatMap allExpr xs
            _ -> []
        )

instance PlayExpr MCaseAlt where
    mapExpr f (MCaseAlt a b) = MCaseAlt (mapExprOpt a) (mapExpr f b)
        where
            mapExprOpt x = mapPredLit g x
            g (MCaseOpt expr ctor) = predLit $ MCaseOpt (mapExpr f expr) ctor
        
    allExpr (MCaseAlt a b) = allExprOpt a ++ allExpr b
        where
            allExprOpt x = map (\(MCaseOpt a b) -> a) $ allPredLit x
    


instance PlayExpr Func where
    mapExpr f x = x{body = mapExpr f (body x)}
    allExpr x = allExpr (body x)

instance PlayExpr Hite where
    mapExpr f x = x{funcs = mapExpr f (funcs x)}
    allExpr x = allExpr (funcs x)


class PlayFunc a where
    mapFunc :: (Func -> Func) -> a -> a

instance PlayFunc a => PlayFunc [a] where
    mapFunc f xs = map (mapFunc f) xs

instance PlayFunc Func where
    mapFunc f x = f x

instance PlayFunc Hite where
    mapFunc f x = x{funcs = mapFunc f (funcs x)}


getVarPath :: Expr -> (FuncArg, [CtorArg])
getVarPath x = f x
    where
        f (Var x) = (x, [])
        f (Sel x y) = (a,b ++ [y])
            where (a,b) = getVarPath x
        f _ = error $ "Hite.Type.getVarPath, called on non-varpath: " ++ show x


-- Generic with function
getWith :: String -> (a -> String) -> [a] -> a
getWith name f xs = case filter (\x -> f x == name) xs of
                        [x] -> x
                        [] -> error $ "getWith: Could not find " ++ name ++ " in " ++ strSet (map f xs)
                        _ -> error $ "getWith: Repetition of " ++ name ++ " in " ++ strSet (map f xs)

-- Get simple items, travel down the tree
getFunc :: Hite -> FuncName -> Func
getFunc hite name = getWith name funcName (funcs hite)


-- Argument functions (these are a bit dubious)
-- 1 based
getArgPos :: FuncName -> FuncArg -> Hite -> Int
getArgPos func arg hite = fromJustNote "Hite.Type.getArgPos" $ lookup arg $ zip (funcArgs (getFunc hite func)) [1..]


getArgName :: FuncName -> Int -> Hite -> FuncArg
getArgName func arg hite = funcArgs (getFunc hite func) !! (arg - 1)


callArg :: Expr -> Int -> Maybe Expr
callArg (Call _ args) n = if length args >= n then Just (args !! (n - 1)) else Nothing


callDepth :: Expr -> Int
callDepth x = maximum $ 0 : [(maximum $ 0 : map callDepth xs) + 1 | Call _ xs <- allExpr x]


-- Id based methods

type Id = [Int]


exprToList :: Expr -> [Expr]
exprToList x = case x of
    Call x xs -> x : xs
    Make _ xs -> xs
    Case x xs -> x : map snd xs
    Sel  x _  -> [x]
    _         -> []


exprFromList :: Expr -> [Expr] -> Expr
exprFromList x ys = case x of
    Call _ _ -> Call (headNote "Hite.Type" ys) (tail ys)
    Make n _ -> Make n ys
    Case _ q -> Case (headNote "Hite.Type" ys) (zip (map fst q) (tail ys))
    Sel  _ p -> let [y] = ys in Sel y p
    n        -> if null ys then n else error "exprFromList, non-empty list"


zipId :: Expr -> [(Id, Expr)]
zipId x = f [] x
    where
        f i x = (i,x) : g i (exprToList x)
        
        g i xs = concatMap (uncurry f) res
            where res = zipWith (\a b -> (a:i,b)) [0..] xs


filterId :: (Expr -> Bool) -> Expr -> [Id]
filterId f x = map fst $ filter (f.snd) $ zipId x


extractId :: Expr -> Id -> Expr
extractId x i = f (reverse i) x
    where
        f [] x = x
        f (i:is) x = f is (exprToList x !! i)


mutateId :: Expr -> Id -> Expr -> Expr
mutateId x i y = f (reverse i) x
    where
        f [] x = y
        f (i:is) x = exprFromList x (rep i res rest)
            where
                res = exprToList x
                rest = f is (res !! i)
        
        rep 0 (x:xs) y = y:xs
        rep n (x:xs) y = x : rep (n-1) xs y


charCtor :: Char -> CtorName
charCtor c = "Char_" ++ (if isAlphaNum c then [c] else pad3 (show (ord c)))
    where pad3 x = replicate (3 - length x) '0' ++ x


blankMCase :: Expr -> Expr
blankMCase x = MCase [MCaseAlt predTrue x]
