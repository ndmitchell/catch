
module Val(
    Val(..), Vals,
    blur,
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
import DataRep



getCtors :: Core -> CoreDataName -> [CoreCtorName]
getCtors core = map coreCtorName . coreDataCtors . coreData core


getFields :: Core -> CoreDataName -> [CoreFieldName]
getFields core = filter (not . isFieldRecursive core) .
                 concatMap (map (fromJust . snd) . coreCtorFields) .
                 coreDataCtors . coreData core


data Val = Val {valCore :: Core, valType :: String, valHead :: ValPart, valTail :: ValPart}
         | Any

data ValPart = ValPart {valCtors :: [Bool], valFields :: [Val]}
               deriving (Eq, Ord, Show)

type Vals = [Val]


instance Eq Val where
    (Val _ a2 a3 a4) == (Val _ b2 b3 b4) = (a2,a3,a4) == (b2,b3,b4)
    Any == Any = True
    _ == _ = False


instance Ord Val where
    compare (Val _ a2 a3 a4) (Val _ b2 b3 b4) = compare (a2,a3,a4) (b2,b3,b4)
    compare Any Any = EQ
    compare Any _ = LT
    compare _ Any = GT


instance Show Val where
    showList xs = showString $ "[" ++ concat (intersperse " | " $ map show xs) ++ "]"

    show Any = "_"
    
    show (Val core typ hd tl) = showPart hd ++ if complete tl then "" else " * " ++ showPart tl
        where
            complete (ValPart ctrs fields) = and ctrs && all (== Any) fields
            
            showPart (ValPart ctrs fields) = showCtrs ctrs ++
                if all (== Any) fields then ""
                else if length ctrs == 1 || length fields <= 1 then showFields fields
                else showFieldsRec fields
        
            showCtrs xs | null res = "0"
                        | otherwise = concat $ intersperse "+" res
                where res = [c | (True,c) <- zip xs (getCtors core typ)]
    
            showFields xs = concatMap ((' ':) . show) xs
    
            showFieldsRec xs = " {" ++ concat (intersperse ", " $ zipWith f xs (getFields core typ)) ++ "}"
                where f x name = name ++ "=" ++ show x


consCore = Core [] [] [CoreData "[]" [] [CoreCtor ":" [("",Just "hd"), ("",Just "tl")], CoreCtor "[]" []]] []



valsTrue = [Any]
valsFalse = []


valsOr :: Core -> Vals -> Vals -> Vals
valsOr core a b = normalise core $ a ++ b


valsOrs :: Core -> [Vals] -> Vals
valsOrs core xs = normalise core $ concat xs


-- not correct yet, doesn't deal with Star/Any properly
-- and doesn't consider Pair T _ ^ Pair _ T == Pair T T
valsAnd :: Core -> Vals -> Vals -> Vals
valsAnd core xs ys = normalise core $ [x | x <- xs, any (x `subsetValue`) ys] ++ [y | y <- ys, any (y `subsetValue`) xs]


valsAnds :: Core -> [Vals] -> Vals
valsAnds core = foldr (valsAnd core) valsTrue



blur :: Core -> Vals -> Vals
blur core vals = normalise core $ vals





-- is a `subset` b
subsetValue :: Val -> Val -> Bool
subsetValue _ Any = True
subsetValue Any _ = False
subsetValue (Val _ _ a1 b1) (Val _ _ a2 b2) = f a1 a2 && f b1 b2
    where
        f (ValPart a1 b1) (ValPart a2 b2) =
            all (uncurry f1) (zip a1 a2) &&
            all (uncurry subsetValue) (zip b1 b2)
        
        f1 x y = x == y || y == True


mergeVal :: Val -> Val -> Val
mergeVal x y = undefined


-- properties of normalise, with X as the result:
--
-- \forall x, y \in X, x \not-subset y
-- if x is a member, it must not be a subset of any other members
--
-- \forall x \in X, y \in \Universe, x \subset y => y \not-subset-eq X
-- if y is a superset of x and is valid in X, then x should not be present
--
normalise :: Core -> Vals -> Vals
normalise core = snub . rule1 -- . rule2
    where
        rule1 :: [Val] -> [Val]
        rule1 xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
            where strictSubset a b = a /= b && a `subsetValue` b
    
    {-     rule2 :: [Val] -> [Val]
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
-}


-- is the root allowed
checkRoot :: Core -> Vals -> CoreCtorName -> Bool
checkRoot core vals name = Any `elem` vals || any f vals
    where
        typ = valType $ head vals
        ind = fromJust $ findIndex (== name) $ getCtors core typ
        f val = valCtors (valHead val) !! ind


integrate :: Core -> Vals -> CoreFieldName -> Vals
integrate core vals field
        | Any `elem` vals = [Any]
        | otherwise = normalise core $ anyCtor core (delete name $ getCtors core typ) ++ map f vals
    where
        typ = coreDataName dat
        dat = coreCtorData core name
        ctr@(CoreCtor name fields) = coreFieldCtor core field
        rec = isFieldRecursive core field
        
        ctrs = coreDataCtors dat
        flds = getFields core typ
        
        (nctrs,nflds) = (length ctrs, length flds)
        
        ictr = fromJust $ findIndex ((== name) . coreCtorName) ctrs
        ifld = fromJust $ findIndex (== field) flds
        
        f v | rec = Val core typ
                               (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any))
                               (ValPart (zipWith (&&) (valCtors $ valHead v) (valCtors $ valTail v))
                                        (zipWith mergeVal (valFields $ valHead v) (valFields $ valTail v)))

            | otherwise = Val core typ
                               (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any !!! (ifld,v)))
                               (ValPart (replicate nctrs True) (replicate nflds Any))



differentiate :: Core -> CoreCtorName -> Vals -> [[Val]]
differentiate core name vals
        | null vals = []
        | Any `elem` vals = [replicate (length $ coreCtorFields ctor) Any]
        | otherwise = [map (f (v,part) . fromJust . snd) $ coreCtorFields ctor 
                      | Val _ _ (ValPart c v) part <- vals, c !! ictr]
    where
        typ = valType $ head vals
        ctrs = getCtors core typ
        flds = getFields core typ
        ctor = coreCtor core name
        ictr = fromJust $ findIndex (==name) ctrs
        
        f (vals,cont) fld
            | isFieldRecursive core fld = Val core typ cont cont
            | otherwise = vals !! fromJust (findIndex (== fld) flds)



-- some simple constructors
anyCtor :: Core -> [CoreCtorName] -> Vals
anyCtor core [] = []
anyCtor core rest = normalise core
        [Val core typ (ValPart (map (`elem` rest) ctrs) fields) (ValPart (replicate (length ctrs) True) fields)]
    where
        typ = coreDataName $ coreCtorData core (head rest)
        ctrs = getCtors core typ
        fields = replicate (length $ getFields core typ) Any
