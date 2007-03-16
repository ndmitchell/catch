
module Analyse.Factorise(factorise) where

import General.General


type Vec a = [a]

-- given a list of items, each being a list of possible choices
-- give the maximum factorisations
-- where multiple ones exist, give more than one
--
-- [[a,b],[a,c]] == [[[a],[b,c]]]
factorise :: Ord a => [Vec a] -> [Vec [a]]
factorise [] = []
factorise (x:xs) = f [map (:[]) x] (map (map (:[])) xs)
    where
        f :: Ord a => [Vec [a]] -> [Vec [a]] -> [Vec [a]]
        f done [] = done
        f done (t:odo) = f (subsetAdd t done) odo
            where t2 = strengthen t (done++odo)


subsetAdd :: Ord a => Vec [a] -> [Vec [a]] -> [Vec [a]]
subsetAdd x [] = [x]
subsetAdd x (y:ys) | x `subset` y = y:ys
                   | y `subset` x = subsetAdd x ys
                   | otherwise = y : subsetAdd x ys


subset :: Ord a => Vec [a] -> Vec [a] -> Bool
subset a b = and $ zipWith subsetList a b

-- is the first sorted list less than the second
subsetList :: Ord a => [a] -> [a] -> Bool
subsetList [] _ = True
subsetList _ [] = False
subsetList (x:xs) (y:ys) =
    case compare x y of
        EQ -> subsetList xs ys
        LT -> False
        GT -> subsetList (x:xs) ys


strengthen :: Ord a => Vec [a] -> [Vec [a]] -> Vec [a]
strengthen x given = foldl f x given
    where
        f :: Ord a => Vec [a] -> Vec [a] -> Vec [a]
        f [] [] = []
        f (x:xs) (y:ys)
            | x `subsetList` y = x : f xs ys
            | and (zipWith subsetList xs ys) = snub (x++y) : xs
            | otherwise = x:xs
