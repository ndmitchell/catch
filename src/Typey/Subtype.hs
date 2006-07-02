
module Typey.Subtype where

import Typey.Type
import Hite
import Data.List
import General.General


data TSubtype = TFree [String]
              | TBind [TPair]
              | TFunc [TArr]
              | TBot
              deriving Eq

data TArr = TArr [TSubtype] TSubtype
            deriving Eq

data TPair = TPair [CtorName] [TSubtype]
             deriving Eq

isTBot (TBot{}) = True; isTBot _ = False

getTArrs :: TSubtype -> [TArr]
getTArrs (TFunc x) = x
getTArrs x = [TArr [] x]


tFunc :: [TArr] -> TSubtype
tFunc [TArr [] x] = x
tFunc x = TFunc x



class PlayTSubtype a where
    mapTSubtype :: (TSubtype -> TSubtype) -> a -> a
    allTSubtype :: a -> [TSubtype]

instance PlayTSubtype a => PlayTSubtype [a] where
    mapTSubtype f xs = map (mapTSubtype f) xs
    allTSubtype xs = concatMap allTSubtype xs


instance PlayTSubtype TSubtype where
    mapTSubtype f x = f $ case x of
        TBind xs -> TBind $ map (mapTSubtype f) xs
        TFunc xs -> TFunc $ map (mapTSubtype f) xs
        _ -> x

    allTSubtype x = x : (case x of
            TBind xs -> concatMap allTSubtype xs
            TFunc xs -> concatMap allTSubtype xs
            _ -> []
        )

instance PlayTSubtype TArr where
    mapTSubtype f (TArr as a) = TArr (map (mapTSubtype f) as) (mapTSubtype f a)
    allTSubtype (TArr as a) = concatMap allTSubtype (as ++ [a])

instance PlayTSubtype TPair where
    mapTSubtype f (TPair a as) = TPair a (map (mapTSubtype f) as)
    allTSubtype (TPair a as) = concatMap allTSubtype as


instance Show TSubtype where
    show (TFree x) = showSet x
    show (TBind xs) = "{" ++ intercatS " | " xs ++ "}"
    show (TFunc x) = "<" ++ intercatS " | " x ++ ">"
    show TBot = "!"

instance Show TArr where
    show (TArr a b) = "(" ++ intercatS " -> " (a++[b]) ++ ")"

instance Show TPair where
    show (TPair a []) = showSet (map repBox a)
    show (TPair a b) = show (TPair a []) ++ " " ++ show b

-- beacuse [] is overloaded in meaning enough already!
repBox "[]" = "#"
repBox x = x

showSet [] = "?"
showSet [x] = x
showSet xs = intercat "'" xs


instance Union TSubtype where
    unionPair (TFree a) (TFree b) = TFree (a `union` b)
    unionPair (TBind a) (TBind b) = TBind (zipWithRest unionPair a b)
    --unionPair (TArr a1 b1) (TArr a2 b2) = tArr (zipWithEq unionPair a1 a2) (b1 `unionPair` b2)
    unionPair TBot _ = TBot
    unionPair _ TBot = TBot
    unionPair (TFree []) x = x
    unionPair x (TFree []) = x
    unionPair a b = error $ show ("Union TSubtype",a,b)

instance Union TPair where
    unionPair (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWithEq unionPair b1 b2)


isTSubset :: TSubtype -> TSubtype -> Bool
isTSubset (TBind xs) (TBind ys) | length xs > length ys = False
                                | otherwise = and $ zipWith isTSubsetPair xs ys
isTSubset TBot TBot = True
isTSubset TBot _ = False
isTSubset (TFree xs) (TFree ys) = null $ xs \\ ys
isTSubset (TFree x) _ = True
isTSubset x y = error $ show ("isTSubset",x,y)


isTSubsetPair (TPair x1 y1) (TPair x2 y2) = f x1 x2 && and (zipWithEq isTSubset y1 y2)
    where
        f xs ys = null $ xs \\ ys


type TypeList = [(String, TSubtype)]


showTypeList :: TypeList -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : g typs
        g (TFunc xs) = map ((++) "    " . show) xs
        g x = ["    " ++ show x]

