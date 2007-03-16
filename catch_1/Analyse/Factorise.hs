
module Analyse.Factorise(factorise) where

import General.General


type Vec a = [a]

-- given a list of items, each being a list of possible choices
-- give the maximum factorisations
-- where multiple ones exist, give more than one
--
-- [[a,b],[a,c]] == [[[a],[b,c]]]
--
-- top is the "highest" element, such that all others are subsets
-- [top] is the only permissable value, since [top,*] == [top]
-- in Catch this is likely to be Any
factorise :: (Show a, Ord a) => a -> [Vec a] -> [Vec [a]]
factorise top xs = f top [] (map (map (:[])) xs)
    where
        f :: (Show a, Ord a) => a -> [Vec [a]] -> [Vec [a]] -> [Vec [a]]
        f top done [] = done
        f top done (t:odo) = f top (subsetAdd top t2 done) odo
            where t2 = strengthen top t (done++odo)


subsetAdd :: (Show a, Ord a) => a -> Vec [a] -> [Vec [a]] -> [Vec [a]]
subsetAdd top x [] = [x]
subsetAdd top x (y:ys)
    | subset top x y = y:ys
    | subset top y x = subsetAdd top x ys
    | otherwise = y : subsetAdd top x ys


subset :: (Show a, Ord a) => a -> Vec [a] -> Vec [a] -> Bool
subset top a b = and $ zipWith (subsetList top) a b

-- is the first sorted list less than the second
subsetList :: Ord a => a -> [a] -> [a] -> Bool
subsetList top [] _ = True
subsetList top _ [] = False
subsetList top _ [t] | t == top = True
subsetList top (x:xs) (y:ys) =
    case compare x y of
        EQ -> subsetList top xs ys
        LT -> False
        GT -> subsetList top (x:xs) ys


strengthen :: (Show a, Ord a) => a -> Vec [a] -> [Vec [a]] -> Vec [a]
strengthen top x given = foldl (f top) x given
    where
        f :: (Show a, Ord a) => a -> Vec [a] -> Vec [a] -> Vec [a]
        f top [] [] = []
        f top (x:xs) (y:ys)
            | subsetList top x y = x : f top xs ys
            | and (zipWith (subsetList top) xs ys) = snub (x++y) : xs
            | otherwise = x:xs
