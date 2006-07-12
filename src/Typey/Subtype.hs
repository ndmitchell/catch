
module Typey.Subtype where

import Typey.Type
import Hite
import Data.List
import General.General


data TSubtype = TFree [String]
              | TBind [TPair]
              | TFunc [TArr]
              | TBot
              | TVoid
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
    show TVoid = "*"

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



type TypeList = [(String, TSubtype)]


showTypeList :: TypeList -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : g typs
        g (TFunc xs) = map ((++) "    " . show) xs
        g x = ["    " ++ show x]

