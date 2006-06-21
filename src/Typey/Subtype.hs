
module Typey.Subtype where

import Typey.Type
import Hite
import Data.List
import General.General


data TSubtype = TFree [String]
              | TBind [TPair]
              | TArr [TSubtype] TSubtype
              | TBot
              deriving Eq

data TPair = TPair [CtorName] [TSubtype]
             deriving Eq

isTBot (TBot{}) = True; isTBot _ = False

instance Show TSubtype where
    show (TFree x) = showSet x
    show (TBind xs) = "{" ++ intercatS " | " xs ++ "}"
    show (TArr a b) = "(" ++ intercatS " -> " (a++[b]) ++ ")"
    show TBot = "!"

instance Show TPair where
    show (TPair a []) = showSet (map repBox a)
    show (TPair a b) = show (TPair a []) ++ " " ++ show b

-- beacuse [] is overloaded in meaning enough already!
repBox "[]" = "#"
repBox x = x

showSet [] = "?"
showSet [x] = x
showSet xs = "<" ++ intercat "," xs ++ ">"


instance Union TSubtype where
    unionPair (TFree a) (TFree b) = TFree (a `union` b)
    unionPair (TBind a) (TBind b) = TBind (zipWithRest unionPair a b)
    unionPair (TArr a1 b1) (TArr a2 b2) = tArr (zipWithEq unionPair a1 a2) (b1 `unionPair` b2)
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


type TypeList = [(String, [TSubtype])]


showTypeList :: TypeList -> String
showTypeList x = unlines $ concatMap f x
    where
        f (name,typs) = (name ++ " ::") : map show typs


tArr [] y = y
tArr xs y = TArr xs y



extractFrees :: [TSubtype] -> [TSubtype]
extractFrees x = concatMap fSubtype x
    where
        fSubtype (TFree a) = [TFree a]
        fSubtype (TBind a) = concatMap fPair a
        fSubtype (TArr a b) = concatMap fSubtype a ++ fSubtype b
        fSubtype (TBot) = []
        
        fPair (TPair a b) = concatMap fSubtype b


replaceFrees :: [TSubtype] -> [TSubtype] -> [TSubtype]
replaceFrees x ns = fSubtypes x ns
    where
        fSubtypes :: [TSubtype] -> [TSubtype] -> [TSubtype]
        fSubtypes [] [] = []
        fSubtypes (x:xs) ns = fSubtype x n1 : fSubtypes xs n2
            where (n1,n2) = splitAt (length $ extractFrees [x]) ns
        fSubtypes x y = error $ show ("fSubtypes",x,y)

            
        fSubtype (TFree a) [n] = n
        fSubtype (TBot) [] = TBot
        fSubtype (TArr a b) n = tArr (init ab) (last ab)
            where ab = fSubtypes (a ++ [b]) n
        fSubtype (TBind xs) n = TBind $ fPairs xs n

        fPairs [] [] = []
        fPairs (TPair a b : xs) ns = TPair a (fSubtypes b n1) : fPairs xs n2
            where (n1,n2) = splitAt (length $ extractFrees b) ns


uniqueFrees :: [TSubtype] -> [TSubtype]
uniqueFrees x = replaceFrees x $ f [] $ extractFrees x
    where
        f rep [] = []
        f rep (TFree [a]:xs) = TFree [a ++ show n] : f ((a,n+1):rep) xs
            where
                n = case lookup a rep of
                        Nothing -> 0
                        Just x -> x
        f rep (x:xs) = x : f rep xs
