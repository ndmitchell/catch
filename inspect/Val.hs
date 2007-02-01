
module Val(
    Val(..), Vals,
    normalise, blur,
    valsAnd, valsOr,
    valsAnds, valsOrs,
    valsTrue, valsFalse,
    anyCtor,
    checkRoot, integrate, differentiate
    ) where


import Yhc.Core
import General
import Data.List
import Data.Maybe


data Val = Val {valCtor :: String, valFields :: [Val]}
         | Any
         | Star
           deriving (Eq, Ord)

instance Show Val where
    show Star = "*"
    show Any = "_"
    show (Val x xs) = ['('|b] ++ unwords (x:map show xs) ++ [')'|b]
        where b = not $ null xs


type Vals = [Val]


consCore = Core [] [] [CoreData "[]" [] [CoreCtor ":" [("",Just "hd"), ("",Just "tl")], CoreCtor "[]" []]] []



valsTrue = [Any]
valsFalse = []


valsOr :: Core -> Vals -> Vals -> Vals
valsOr core a b = a ++ b


valsOrs :: Core -> [Vals] -> Vals
valsOrs core xs = concat xs


-- not correct yet, doesn't deal with Star/Any properly
-- and doesn't consider Pair T _ ^ Pair _ T == Pair T T
valsAnd :: Core -> Vals -> Vals -> Vals
valsAnd core xs ys = [x | x <- xs2, any (x `subsetValue`) ys2] ++ [y | y <- ys2, any (y `subsetValue`) xs2]
    where (xs2,ys2) = (normalise core xs, normalise core ys)


valsAnds :: Core -> [Vals] -> Vals
valsAnds core = foldr (valsAnd core) valsTrue



blur :: Core -> Vals -> Vals
blur core vals = vals





-- is a `subset` b
subsetValue :: Val -> Val -> Bool
subsetValue _ Any = True
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
normalise :: Core -> Vals -> Vals
normalise core = rule1 . rule2
    where
        rule1 :: [Val] -> [Val]
        rule1 xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
            where strictSubset a b = a /= b && a `subsetValue` b
    
        rule2 :: [Val] -> [Val]
        rule2 [] = []
        rule2 xs | Any `elem` xs = [Any]
                 | all isValueStar groups && snub (map valCtor groups) == snub ctors = [Any]
                 | otherwise = groups
            where
                ctors = map coreCtorName $ coreDataCtors $ coreCtorData core $ valCtor $ head xs
                groups = concatMap regroup $ groupSortBy cmpValCtor xs
                
                isValueStar (Val _ xs) = all (== Any) xs
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



-- is the root allowed
checkRoot :: Core -> Vals -> CoreCtorName -> Bool
checkRoot core vals name = Star `elem` vals || any (== name) (map valCtor vals)


integrate :: Core -> Vals -> CoreFieldName -> Vals
integrate core vals field = [f c | c <- coreDataCtors dat, coreCtorName c /= name] ++ map g vals
    where
        dat = coreCtorData core name
        ctr@(CoreCtor name fields) = coreFieldCtor core field
        
        f (CoreCtor name fields) = Val name (replicate (length fields) Any)
        g x = Val name [if i == field then x else Any | (_, Just i) <- fields]


differentiate :: Core -> Vals -> CoreFieldName -> Vals
differentiate core vals field
        | Star `elem` vals = [Star]
        | otherwise = [cs !! ind | Val n cs <- vals, n == name]
    where
        ind = fromJust $ findIndex (==field) (map (fromJust . snd) fields)
        ctr@(CoreCtor name fields) = coreFieldCtor core field


-- some simple constructors
anyCtor :: Core -> [CoreCtorName] -> Vals
anyCtor core = map f
    where
        f name = Val name (replicate (length fields) Any)
            where fields = coreCtorFields $ coreCtor core name
