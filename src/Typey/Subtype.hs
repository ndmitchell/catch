
module Typey.Subtype where

import Typey.Type
import Hite
import Data.List
import General.General


data TSubtype = TFree [String]
              | TBind [TPair]
              | TArr [TSubtype] TSubtype
              | TBot

data TPair = TPair [CtorName] [TSubtype]

isTBot (TBot{}) = True; isTBot _ = False

instance Show TSubtype where
    show (TFree x) = showSet x
    show (TBind xs) = "{" ++ (concat $ intersperse " | " $ map show xs) ++ "}"
    show (TArr a b) = "(" ++ (concat $ intersperse " -> " $ map show (a++[b])) ++ ")"
    show TBot = "!"

instance Show TPair where
    show (TPair a b) = showSet (map repBox a) ++ " " ++ show b

-- beacuse [] is overloaded in meaning enough already!
repBox "[]" = "#"
repBox x = x

showSet [] = "?"
showSet [x] = x
showSet xs = "[" ++ (concat $ intersperse "," $ map show xs) ++ "]"


instance Union TSubtype where
    unionPair (TFree a) (TFree b) = TFree (a `union` b)
    unionPair (TBind a) (TBind b) = TBind (zipWithEq unionPair a b)
    unionPair (TArr a1 b1) (TArr a2 b2) = TArr (zipWithEq unionPair a1 a2) (b1 `unionPair` b2)
    unionPair a b = error $ show ("Union TSubtype",a,b)

instance Union TPair where
    unionPair (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWithEq unionPair b1 b2)


isTSubset :: TSubtype -> TSubtype -> Bool
isTSubset (TBind xs) (TBind ys) | length xs > length ys = False
                                | otherwise = and $ zipWith isTSubsetPair xs ys
isTSubset TBot TBot = True
isTSubset TBot _ = False
isTSubset (TFree _) _ = True
isTSubset x y = error $ show ("isTSubset",x,y)


isTSubsetPair (TPair x1 y1) (TPair x2 y2) = f x1 x2 && and (zipWithEq isTSubset y1 y2)
    where
        f xs ys = null $ xs \\ ys


type TypeList = [(String, [TSubtype])]


showTypeList :: TypeList -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : map show typs


tArr [] y = y
tArr xs y = TArr xs y
