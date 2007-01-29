
module PathCtorEq(
    --Val, equalValue,
    --enumeratePathCtor, equalPathCtor,
    --enumeratePathCtorProp, equalPathCtorProp,
    --normalise
    ) where

import PathCtor
import Path
import General
import Data.List
import Data.Maybe
import Data.Proposition
import Yhc.Core


data Val = Val {valCtor :: String, valFields :: [Val]}
         | Star
           deriving (Eq, Ord)


instance Show Val where
    show Star = "*"
    show (Val x xs) = ['('|b] ++ unwords (x:map show xs) ++ [')'|b]
        where b = not $ null xs


-- allValue = [("A",A Star Star), ("B",B Star), ("C",C Star), ("D",D)]


enumeratePathCtor :: PathCtor -> [Val]
enumeratePathCtor (PathCtor core (Path []) ctor) =
    [Val c (replicate arity Star) | c <- ctor, let arity = length $ coreCtorFields $ coreCtor core c]

enumeratePathCtor (PathCtor core (Path (PathAtom x:xs)) ctor) =
        [Val name (replicate arity Star) | c <- coreDataCtors dat, x `notElem` map (fromJust . snd) (coreCtorFields c),
                                           let name = coreCtorName c, let arity = length $ coreCtorFields c]
        ++
        [Val (coreCtorName ctr) children | children <- sequence (map (f . fromJust . snd) (coreCtorFields ctr))]
    where
        ctr = coreFieldCtor core x
        dat = coreCtorData core (coreCtorName ctr)
        
        f field | field == x = enumeratePathCtor (PathCtor core (Path xs) ctor)
                | otherwise  = [Star]

enumeratePathCtor (PathCtor core (Path (PathStar x:xs)) ctor) = concatMap f base
    where
        f c = [Val (coreCtorName c) children | children <- sequence (map (g . fromJust . snd) (coreCtorFields c))]
        
        g p = g2 p (PathStar x:xs)
        
        g2 p (PathAtom x:xs) | p == x = enumeratePathCtor (PathCtor core (Path xs) ctor)
        g2 p (PathStar x:xs) | p `elem` x = enumeratePathCtor (PathCtor core (Path xs) ctor)
                             | otherwise = g2 p xs
        g2 p _ = [Star]
        
        base = if null xs
               then map (coreCtor core) ctor
               else coreDataCtors $ coreFieldData core $ head x

{-
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
-}

enumeratePathCtorProp :: Prop p => p PathCtor -> [Val]
enumeratePathCtorProp p = propFold fold p
    where
        fold = PropFold {foldOr = ors, foldAnd = ands, foldLit = enumeratePathCtor}
        
        ors x = snub $ concat x
        
        ands [] = [Star]
        ands [xs,ys] = [x | x <- xs2, any (x `subsetValue`) ys2] ++ [y | y <- ys2, any (y `subsetValue`) xs2]
            where (xs2,ys2) = (normalise xs, normalise ys)
            
        ands x = error $ show ("ands",x)


-- is a `subset` b
subsetValue :: Val -> Val -> Bool
subsetValue _ Star = True
subsetValue (Val a as) (Val b bs) = a == b && f as bs
    where
        f [] [] = True
        f (x:xs) (y:ys) = x `subsetValue` y && f xs ys
        f _ _ = False
subsetValue _ _ = False


normalise :: [Val] -> [Val]
normalise = undefined

{-
normalise xs = if Star `elem` xs || all ((`elem` res) . snd) allValue then [Star] else res
    where
        res :: [Val]
        res = snub $ concat $ map (\x -> f (fst $ head x) (map snd x)) $
              groupSortBy cmpFst [(tagChar a,a) | a <- xs]

        fromB (B x) = x
        fromC (C x) = x
        fromA (A a b) = (a,b)
        swap (a,b) = (b,a)
        
        f :: Char -> [Val] -> [Val]
        f 'A' = map (uncurry A) . g . map fromA
        f 'B' = map B . normalise . map fromB
        f 'C' = map C . normalise . map fromC
        f 'D' = id
        
        
        g :: [(Val, Val)] -> [(Val, Val)]
        g x = if x2 == x then x else g x2
            where x2 = snub $ map swap $ h $ map swap $ h x
        
        h :: [(Val, Val)] -> [(Val,Val)]
        h = concat . map (\x -> map ((,) (fst $ head x)) $ normalise $ map snd x) . groupSortBy cmpFst


normaliseMore :: [Val] -> [Val]
normaliseMore xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
    where strictSubset a b = a /= b && a `subsetValue` b


groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\a b -> f a b == EQ) . sortBy f


cmpFst (a,_) (b,_) = compare a b


equalValue :: [Val] -> [Val] -> Bool
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

-}
