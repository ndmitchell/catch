
{-|
    A signature for the abstract type
-}

module Hite.Type where

import General
import Maybe
import RegExp


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
          | Path {expr :: Expr, pathPath :: RegExp CtorArg}
          | CallFunc {callName :: FuncName}
          | Make {makeName :: CtorName, makeArgs :: [Expr]}
          | Case Expr [(CtorName, Expr)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])
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
        Path a b  -> Path (mapExpr f a) b
        _ -> x
    
    allExpr x = x : concatMap allExpr (case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Case _ xs -> map snd xs
            Sel  x _  -> [x]
            Path x _  -> [x]
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



selToPath :: Expr -> Expr
selToPath x = mapExpr f x
    where
        f (Sel (Path a b) c) = Path a (regConcat [b,regLit c])
        f (Sel a b) = Path a (regLit b)
        f x = x

{-



allExpr :: Expr -> [Expr]
allExpr x = x : concatMap allExpr (case x of
        Call x xs -> x : xs
        Make _ xs -> xs
        Case _ xs -> map snd xs
        _ -> []
    )

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f x = f $ case x of
    Call a bs -> Call (mapExpr f a) (map (mapExpr f) bs)
    Make a bs -> Make a             (map (mapExpr f) bs)
    Case a bs -> Case (mapExpr f a) (map (\(d,e) -> (d,mapExpr f e)) bs)
    _ -> x

-}
{-

mapExprHite :: (Expr -> Expr) -> Hite -> Hite
mapExprHite f (Hite a b) = Hite a (map (mapExprFunc f) b)


mapExprFunc :: (Expr -> Expr) -> Func -> Func
mapExprFunc f func = func{body = mapExpr f (body func)}
-}


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
getDataFromCtor name hite = head $ filter f (datas hite)
    where f (Data _ ctors) = name `elem` (map ctorName ctors)


-- 1 based
getArgPos :: FuncName -> FuncArg -> Hite -> Int
getArgPos func arg hite = fromJust $ lookup arg $ zip (funcArgs (getFunc func hite)) [1..]


getArgName :: FuncName -> Int -> Hite -> FuncArg
getArgName func arg hite = funcArgs (getFunc func hite) !! (arg - 1)


callArg :: Expr -> Int -> Maybe Expr
callArg (Call _ args) n = if length args >= n then Just (args !! (n - 1)) else Nothing


{- 

type Data = [[CtorName]]

data Sel = Sel {selName :: CtorName, selNum :: Int}
           deriving (Eq)


data Exp = Call {callFunc :: Exp, callArgs :: [Exp]}
         | Var Int [Sel] -- variable, the path you have to go down
                         -- all variables are parameter based, 1 indexed
         | CallFunc {callName :: String}
         | Make {makeName :: CtorName, makeArgs :: [Exp]}
         | Case Exp [(CtorName, Exp)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])
         
         -- arggghh, the world just became higher order
         | CallHO Exp [Exp]
           deriving (Show, Eq)


-- HITE MANIPULATION FUNCTIONS

instance Show Sel where
    show (Sel a n) | a == "Cons" || a == ":" =
                   case n of
                       1 -> "head"
                       2 -> "tail"
                       x -> "(ERROR Cons:" ++ show x ++ ")"
    
    show (Sel a b) = a ++ "#" ++ show b


varAdd :: Exp -> String -> Int -> Exp
varAdd (Var n xs) str int = Var n (xs ++ [Sel str int])
varAdd x _ _ = error $ "varAdd: Exp not a variable (" ++ show x ++ ")"


-- given one constructor, list all the constructors of that type
allCtors :: Hite -> CtorName -> [CtorName]
allCtors hite ctor = concat $ filter (ctor `elem`) (datas hite)


isCase (Case _ _) = True ; isCase _ = False
isCall (Call _ _) = True ; isCall _ = False
isVar (Var _ _) = True ; isVar _ = False
isMake (Make _ _) = True ; isMake _ = False


allExps :: Exp -> [Exp]
allExps x = x : concatMap allExps (case x of
        Call x xs -> x : xs
        Make _ xs -> xs
        Case _ xs -> snds xs
        _ -> []
    )
    
allExpsFunc hite func = allExps (getFunc hite func)



isCaseComplete :: Hite -> Exp -> Bool
isCaseComplete hite (Case _ stmts) = null (allCtors hite (head ctors) \\ ctors)
    where ctors = fsts stmts



listHigherOrder :: Hite -> [String]
listHigherOrder hite = map fst (filter f (funcs hite))
    where
        f (name, expr) = any g (allExps expr)
        g (Call (CallFunc _) _) = False
        g (Call _ _) = True
        g _ = False



-- represent call's which have got dropped to one end
callLift :: Hite -> Hite
callLift hite = hite{funcs = map (\(a,b) -> (a, mapExp f b)) (funcs hite)}
    where
        f (Call (Call a b) c) = Call a (b++c)
        f x = x

-}
