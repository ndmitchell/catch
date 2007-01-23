
module PathCtorEq(Value, enumerate, equal, equalPathCtor) where

import PathCtor hiding (equalPathCtor)
import Path
import General
import Data.List
import Data.Maybe
import Data.Play

data Value = A Value Value
           | B Value
           | C Value
           | D
           | Star
           | Complete String
           deriving (Show, Eq, Ord)

instance Play Value where
    replaceChildren x = case x of
        A a as -> playTwo A a as
        B bs -> playOne B bs
        C c -> playOne C c
        x -> playDefault x


allValue = [("A",A Star Star), ("B",B Star), ("C",C Star), ("D",D)]


enumerate :: PathCtor -> [Value]
enumerate (PathCtor core path ctor) = concatMap f base
    where
        f (A x y) = [A x2 y2 | x2 <- g "a", y2 <- g "as"]
        f (B x  ) = [B x2    | x2 <- g "bs"]
        f (C x  ) = [C x2    | x2 <- g "c"]
        f x = [x]

        g p = g2 p (fromPath path)
        
        g2 p (PathAtom x:xs) | p == x = enumerate (PathCtor core (Path xs) ctor)
        g2 p (PathStar x:xs) | p `elem` x = enumerate (PathCtor core (Path xs) ctor)
                             | otherwise = g2 p xs
        g2 p _ = [Star]
    
        base = if ewpPath path
               then if length ctor == 4 then [Star] else [b | (a,b) <- allValue, a `elem` ctor]
               else map snd allValue

normalise :: [Value] -> [Value]
normalise values = if values == subval then values else normalise subval
    where
        subval = filter (\x -> not $ any (\y -> x /= y && subset x y) newval) newval
    
        newval = snub $ map (mapUnder removeComplete) $ concat $ map mergeComplete $
                 groupBy eqComplete $ sortBy cmpComplete $ map (mapUnder useComplete) values
    
        useComplete (A Star Star) = Complete "A"
        useComplete (B Star) = Complete "B"
        useComplete (C Star) = Complete "C"
        useComplete D = Complete "D"
        useComplete x = x
        
        mergeComplete (x:xs) = f (allOverContext x) : xs
            where
                f ((me,rebuild):rest)
                    | all (`elem` (x:xs)) $ map (rebuild . Complete) ["A","B","C","D"] = rebuild Star
                    | otherwise = f rest
                f [] = x

        removeComplete (Complete c) = fromJust $ lookup c allValue
        removeComplete x = x
        
        -- is x a subset of y
        subset x Star = True
        subset (A a1 as1) (A a2 as2) = a1 `subset` a2 && as1 `subset` as2
        subset (B bs1) (B bs2) = bs1 `subset` bs2
        subset (C c1) (C c2) = c1 `subset` c2
        subset x y = x == y


eqComplete (A a1 as1) (A a2 as2) = eqComplete a1 a2 && eqComplete as1 as2
eqComplete (B bs1) (B bs2) = eqComplete bs1 bs2
eqComplete (C c1) (C c2) = eqComplete c1 c2
eqComplete (Complete _) (Complete _) = True
eqComplete x y = x == y

cmpComplete a b | tag a /= tag b = compare (tag a) (tag b)
cmpComplete (A a1 as1) (A a2 as2) = cmpComplete a1 a2 `next` cmpComplete as1 as2
    where next a b = if a == EQ then b else a
cmpComplete (B bs1) (B bs2) = cmpComplete bs1 bs2
cmpComplete (C c1) (C c2) = cmpComplete c1 c2
cmpComplete _ _ = EQ


tag (A{}) = 0
tag (B{}) = 1
tag (C{}) = 2
tag (D{}) = 3
tag (Star{}) = 4
tag (Complete{}) = 5



normalise2 :: [Value] -> [Value]
normalise2 xs = if Star `elem` xs || all ((`elem` res) . snd) allValue then [Star] else res
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
        f 'B' = map B . normalise2 . map fromB
        f 'C' = map C . normalise2 . map fromC
        f 'D' = id
        
        
        g :: [(Value, Value)] -> [(Value, Value)]
        g x = if x2 == x then x else g x2
            where x2 = snub $ map swap $ h $ map swap $ h x
        
        h :: [(Value, Value)] -> [(Value,Value)]
        h = concat . map (\x -> map ((,) (fst $ head x)) $ normalise2 $ map snd x) . groupSortBy cmpFst


groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\a b -> f a b == EQ) . sortBy f


cmpFst (a,_) (b,_) = compare a b

tagChar (A{}) = 'A'
tagChar (B{}) = 'B'
tagChar (C{}) = 'C'
tagChar (D{}) = 'D'



equal :: [Value] -> [Value] -> Bool
equal a b | a == b = True
equal a b = normalise2 a == normalise2 b


equalPathCtor :: PathCtor -> PathCtor -> Bool
equalPathCtor a b = enumerate a `equal` enumerate b
