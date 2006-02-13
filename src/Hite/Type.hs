
{-|
    A signature for the abstract type
-}

module Hite.Type where

import General.General
import Maybe


type FuncName = String
type DataName = String
type CtorName = String
type CtorArg  = String
type FuncArg  = String

data Hite = Hite {datas :: [Data], funcs :: [Func]}

data Data = Data {dataName :: DataName, ctors :: [Ctor]}

data Ctor = Ctor {ctorName :: CtorName, ctorArgs :: [CtorArg]}
          deriving Eq

data Func = Func {funcName :: FuncName, funcArgs :: [FuncArg], body :: Expr, funcKind :: Kind}
          deriving Eq

data Kind = Star
          | Arrow [Kind]
          | Kinded [([CtorArg], Kind)]
          deriving Eq

data Expr = Call {callFunc :: Expr, callArgs :: [Expr]}
          | Var {varArg :: FuncArg, scope :: FuncName}
          | Sel {expr :: Expr, path :: CtorArg}
          | CallFunc {callName :: FuncName}
          | Make {makeName :: CtorName, makeArgs :: [Expr]}
          | Case Expr [(CtorName, Expr)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])
          
          -- constructor, list of arguments (nothing is recurse), expr is alt
          | Repeat {expr :: Expr, alt :: Expr}
          | RepeatNow
          | Bottom
          deriving Eq


isVar (Var{}) = True; isVar _ = False
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
        _ -> x
    
    allExpr x = x : concatMap allExpr (case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Case x xs -> x : map snd xs
            Sel  x _  -> [x]
            _ -> []
        )

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



getWith :: String -> (a -> String) -> [a] -> a
getWith name f xs = case filter (\x -> f x == name) xs of
                        [x] -> x
                        [] -> error $ "getWith: Could not find " ++ name ++ " in " ++ strSet (map f xs)
                        _ -> error $ "getWith: Repetition of " ++ name ++ " in " ++ strSet (map f xs)

getFunc :: FuncName -> Hite -> Func
getFunc name hite = getWith name funcName (funcs hite)

getData :: DataName -> Hite -> Data
getData name hite = getWith name dataName (datas hite)

getCtor :: CtorName -> Hite -> Ctor
getCtor name hite = getWith name ctorName (concatMap ctors (datas hite))


getDataFromCtor :: CtorName -> Hite -> Data
getDataFromCtor name hite = case filter f (datas hite) of
        [x] -> x
        _ -> error $ "Hite.Type.getDataFromCtor, could not find type with ctor name: " ++ name
    where f (Data _ ctors) = name `elem` (map ctorName ctors)


getCtorFromArg :: CtorArg -> Hite -> Ctor
getCtorFromArg name hite = head [c | d <- datas hite, c <- ctors d, name `elem` ctorArgs c]


-- 1 based
getArgPos :: FuncName -> FuncArg -> Hite -> Int
getArgPos func arg hite = fromJust $ lookup arg $ zip (funcArgs (getFunc func hite)) [1..]


getArgName :: FuncName -> Int -> Hite -> FuncArg
getArgName func arg hite = funcArgs (getFunc func hite) !! (arg - 1)


callArg :: Expr -> Int -> Maybe Expr
callArg (Call _ args) n = if length args >= n then Just (args !! (n - 1)) else Nothing
