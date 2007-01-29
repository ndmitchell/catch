
module PathCtorEq(
    Val, equalValue,
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
import Yhc.Core


data Val = Val {valCtor :: String, valFields :: [Val]}
         | Star
           deriving (Eq, Ord)


instance Show Val where
    show Star = "*"
    show (Val x xs) = ['('|b] ++ unwords (x:map show xs) ++ [')'|b]
        where b = not $ null xs



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
        
        g2 p (PathAtom x:xs) | p == x = enumeratePathCtor $ PathCtor core (Path xs) ctor
        g2 p (PathStar x:xs) | [p] == x = enumeratePathCtor $ PathCtor core (Path xs) ctor
                             | p `elem` x = enumeratePathCtor $ PathCtor core (Path (PathStar (delete p x):xs)) ctor
                             | otherwise = g2 p xs
        g2 p _ = [Star]
        
        base = if null xs
               then map (coreCtor core) ctor
               else coreDataCtors $ coreFieldData core $ head x


grabCore :: Prop p => [p PathCtor] -> Core
grabCore ps = case concatMap propAll ps of
                  (PathCtor core _ _:_) -> core
                  _ -> error "PathCtorEq.grabCore, no core in the predicate but asked for"


enumeratePathCtorProp :: Prop p => p PathCtor -> [Val]
enumeratePathCtorProp p = propFold fold p
    where
        fold = PropFold {foldOr = ors, foldAnd = ands, foldLit = enumeratePathCtor}
        
        ors x = snub $ concat x
        
        ands [] = [Star]
        ands [xs,ys] = [x | x <- xs2, any (x `subsetValue`) ys2] ++ [y | y <- ys2, any (y `subsetValue`) xs2]
            where (xs2,ys2) = (normalise core xs, normalise core ys)

        ands x = error $ show ("ands",x)

        core = grabCore [p]


-- is a `subset` b
subsetValue :: Val -> Val -> Bool
subsetValue _ Star = True
subsetValue (Val a as) (Val b bs) = a == b && f as bs
    where
        f [] [] = True
        f (x:xs) (y:ys) = x `subsetValue` y && f xs ys
        f _ _ = False
subsetValue _ _ = False




-- properties of normalise, with X as the result:
--
-- \forall x, y \in X, x \not-subset y
-- if x is a member, it must not be a subset of any other members
--
-- \forall x \in X, y \in \Universe, x \subset y => y \not-subset-eq X
-- if y is a superset of x and is valid in X, then x should not be present
--
normalise :: Core -> [Val] -> [Val]
normalise core = rule1 . rule2
    where
        rule1 :: [Val] -> [Val]
        rule1 xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
            where strictSubset a b = a /= b && a `subsetValue` b
    
        rule2 :: [Val] -> [Val]
        rule2 [] = []
        rule2 xs | Star `elem` xs = [Star]
                 | all isValueStar groups && snub (map valCtor groups) == snub ctors = [Star]
                 | otherwise = groups
            where
                ctors = map coreCtorName $ coreDataCtors $ coreCtorData core $ valCtor $ head xs
                groups = concatMap regroup $ groupSortBy cmpValCtor xs
                
                isValueStar (Val _ xs) = all (== Star) xs
                isValueStar _ = False
                

        -- all the Val's have the same valCtor
        regroup :: [Val] -> [Val]
        regroup xs | arity == 0 = [Val name []]
                   | arity == 1 = map (\x -> Val name [x]) $ rule2 $ map (head . valFields) xs
                   | otherwise = map (Val name) $ fix operate $ map valFields xs
            where
                arity = length children
                Val name children = head xs
                
                operate = snub . apply (map f [0..arity-1])
                    where
                        f n x = map (retract n) $ concat $ map g $ groupSortBy cmpSnd $ map (extract n) x
                        g ys = map (\x -> (x, snd $ head ys)) $ rule2 $ map fst ys


        groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
        groupSortBy f = groupBy (\a b -> f a b == EQ) . sortBy f

        cmpFst a b = compare (fst a) (fst b)
        cmpSnd a b = compare (snd a) (snd b)
        cmpValCtor a b = compare (valCtor a) (valCtor b)

        apply [] x = x
        apply (f:fs) x = apply fs (f x)

        fix f x = if x == x2 then x else fix f x2
            where x2 = f x

        extract n xs = (xs !! n, take n xs ++ drop (n+1) xs)
        retract n (x,xs) = take n xs ++ [x] ++ drop n xs


equalValue :: Core -> [Val] -> [Val] -> Bool
equalValue core a b | a == b = True
                    | otherwise = normalise core a == normalise core b


equalPathCtor :: Core -> PathCtor -> PathCtor -> Bool
equalPathCtor core a b = equalValue core (enumeratePathCtor a) (enumeratePathCtor b)


equalPathCtorProp :: Prop p => p PathCtor -> p PathCtor -> Bool
equalPathCtorProp a b = equalValue core (enumeratePathCtorProp a) (enumeratePathCtorProp b)
    where core = grabCore [a,b]
