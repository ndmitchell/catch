
module PathCtorEqABCD(
    Value, equalValue,
    enumeratePathCtor, equalPathCtor,
    enumeratePathCtorProp, equalPathCtorProp,
    normalise
    ) where

import PathCtor
import Path
import General
import Data.List
import Data.Maybe
import Data.Proposition


data Value = A Value Value
           | B Value
           | C Value
           | D
           | Star
           deriving (Show, Eq, Ord)


allValue = [("A",A Star Star), ("B",B Star), ("C",C Star), ("D",D)]


enumeratePathCtor :: PathCtor -> [Value]
enumeratePathCtor (PathCtor core path ctor) = concatMap f base
    where
        f (A x y) = [A x2 y2 | x2 <- g "a", y2 <- g "as"]
        f (B x  ) = [B x2    | x2 <- g "bs"]
        f (C x  ) = [C x2    | x2 <- g "c"]
        f x = [x]

        g p = g2 p (fromPath path)
        
        g2 p (PathAtom x:xs) | p == x = enumeratePathCtor (PathCtor core (Path xs) ctor)
        g2 p (PathStar x:xs) | p `elem` x = enumeratePathCtor (PathCtor core (Path xs) ctor)
                             | otherwise = g2 p xs
        g2 p _ = [Star]
    
        base = if ewpPath path
               then if length ctor == 4 then [Star] else [b | (a,b) <- allValue, a `elem` ctor]
               else map snd allValue

enumeratePathCtorProp :: Prop p => p PathCtor -> [Value]
enumeratePathCtorProp p = propFold fold p
    where
        fold = PropFold {foldOr = ors, foldAnd = ands, foldLit = enumeratePathCtor}
        
        ors x = snub $ concat x
        
        ands [] = [Star]
        ands [xs,ys] = [x | x <- xs2, any (x `subsetValue`) ys2] ++ [y | y <- ys2, any (y `subsetValue`) xs2]
            where (xs2,ys2) = (normalise xs, normalise ys)
            
        ands x = error $ show ("ands",x)


-- is a `subset` b
subsetValue :: Value -> Value -> Bool
subsetValue _ Star = True
subsetValue (A a1 as1) (A a2 as2) = a1 `subsetValue` a2 && as1 `subsetValue` as2
subsetValue (B bs1) (B bs2) = bs1 `subsetValue` bs2
subsetValue (C c1) (C c2) = c1 `subsetValue` c2
subsetValue D D = True
subsetValue _ _ = False


normalise :: [Value] -> [Value]
normalise xs = if Star `elem` xs || all ((`elem` res) . snd) allValue then [Star] else res
    where
        res :: [Value]
        res = snub $ concat $ map (\x -> f (fst $ head x) (map snd x)) $
              groupSortBy cmpFst [(tagChar a,a) | a <- xs]

        fromB (B x) = x
        fromC (C x) = x
        fromA (A a b) = (a,b)
        swap (a,b) = (b,a)
        
        f :: Char -> [Value] -> [Value]
        f 'A' = map (uncurry A) . g . map fromA
        f 'B' = map B . normalise . map fromB
        f 'C' = map C . normalise . map fromC
        f 'D' = id
        
        
        g :: [(Value, Value)] -> [(Value, Value)]
        g x = if x2 == x then x else g x2
            where x2 = snub $ map swap $ h $ map swap $ h x
        
        h :: [(Value, Value)] -> [(Value,Value)]
        h = concat . map (\x -> map ((,) (fst $ head x)) $ normalise $ map snd x) . groupSortBy cmpFst


normaliseMore :: [Value] -> [Value]
normaliseMore xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
    where strictSubset a b = a /= b && a `subsetValue` b


groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\a b -> f a b == EQ) . sortBy f


cmpFst (a,_) (b,_) = compare a b

tagChar (A{}) = 'A'
tagChar (B{}) = 'B'
tagChar (C{}) = 'C'
tagChar (D{}) = 'D'



equalValue :: [Value] -> [Value] -> Bool
equalValue a b | a == b = True
               | na == nb = True
               | otherwise = normaliseMore na == normaliseMore nb
    where
        na = normalise a
        nb = normalise b


equalPathCtor :: PathCtor -> PathCtor -> Bool
equalPathCtor a b = enumeratePathCtor a `equalValue` enumeratePathCtor b


equalPathCtorProp :: Prop p => p PathCtor -> p PathCtor -> Bool
equalPathCtorProp a b = enumeratePathCtorProp a `equalValue` enumeratePathCtorProp b

