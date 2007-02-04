
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
    
            showFields xs = concatMap (\x -> " {" ++ show x ++ "}") xs
    
            showFieldsRec xs = " {" ++ concat (intersperse ", " $ zipWith f xs (getFields core typ)) ++ "}"
                where f x name = name ++ "=" ++ show x


consCore = Core [] [] [CoreData "[]" [] [CoreCtor ":" [("",Just "hd"), ("",Just "tl")], CoreCtor "[]" []]] []



valsTrue = [Any]
valsFalse = []


valsOr :: Core -> Vals -> Vals -> Vals
valsOr core a b = normalise core $ a ++ b


valsOrs :: Core -> [Vals] -> Vals
valsOrs core xs = normalise core $ concat xs


valsAnd :: Core -> Vals -> Vals -> Vals
valsAnd core xs ys = normalise core [mergeAnd x y | x <- xs, y <- ys]


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


-- SHOULD THIS BE MERGE_AND OR MERGE_OR ?
mergeVal :: Val -> Val -> Val
mergeVal x y = error "Todo, Val.mergeVal"


-- a `mergeAnd` b = c
-- c `subsetValue` a && c `subsetValue` b
mergeAnd :: Val -> Val -> Val
mergeAnd Any x = x
mergeAnd x Any = x
mergeAnd (Val core typ a1 a2) (Val _ _ b1 b2) = Val core typ (f a1 b1) (f a2 b2)
    where f (ValPart a1 a2) (ValPart b1 b2) = ValPart (zipWith (&&) a1 b1) (zipWith mergeAnd a2 b2)


-- properties of normalise, with X as the result:
--
-- \forall x, y \in X, x \not-subset y
-- if x is a member, it must not be a subset of any other members
--
-- \forall x \in X, y \in \Universe, x \subset y => y \not-subset-eq X
-- if y is a superset of x and is valid in X, then x should not be present
--
normalise :: Core -> Vals -> Vals
normalise core = snub . ruleSubset . ruleCombine . ruleIntroduce . ruleIndividual
    where
        -- combine pairs into one
        ruleCombine :: [Val] -> [Val]
        ruleCombine vals = f [] vals
            where
                f done [] = done
                f done (t:odo) = case g t odo of
                                    Nothing -> f (t:done) odo
                                    Just new -> f [] (done ++ new)

                g y xs = do
                        ind <- findIndex isJust res
                        return $ fromJust (res !! ind) : (xs \!! ind)
                    where
                        item = fromJust $ findIndex isJust res
                        res = map (joinOr y) xs


        -- joinOr a b = Just c, a v b == c
        joinOr :: Val -> Val -> Maybe Val
        joinOr Any _ = Just Any
        joinOr _ Any = Just Any
        joinOr (Val core typ a1 a2) (Val _ _ b1 b2) | a2 == b2 =
            do c1 <- joinPartOr a1 b1; return $ Val core typ c1 a2
        joinOr _ _ = Nothing

        joinPartOr (ValPart a1 a2) (ValPart b1 b2)
            | a2 == b2 = Just $ ValPart (zipWith (||) a1 b1) a2
            | a1 == b1 = listToMaybe [ValPart a1 (pre1++[x]++post2)
                                     | ((pre1,x1,post1),(pre2,x2,post2)) <- zip (allItems a2) (allItems b2)
                                     , pre1 == pre2, post1 == post2, Just x <- [joinOr x1 x2]]
            | otherwise = Nothing


        -- do not allow two where one is a subset of the other
        ruleSubset :: [Val] -> [Val]
        ruleSubset xs = filter (\y -> not $ any (y `strictSubset`) xs) xs
            where strictSubset a b = a /= b && a `subsetValue` b


        -- introduce new terms which may be supersets
        ruleIntroduce :: [Val] -> [Val]
        ruleIntroduce xs = xs ++ [f a b | a <- xs, b <- xs]
            where
                f (Val core typ (ValPart a1 b1) (ValPart c1 d1))
                  (Val _    _   (ValPart a2 b2) (ValPart c2 d2)) =
                  Val core typ (ValPart (zipWith (||) a1 a2) (zipWith mergeAnd b1 b2))
                               (ValPart (zipWith (&&) c1 c2) (zipWith mergeAnd d1 d2))


        -- reduce each rule in isolation
        ruleIndividual :: [Val] -> [Val]
        ruleIndividual = concatMap f
            where
                f x | isBlank x2 = []
                    | isComplete x2 = [Any]
                    | otherwise = [x2]
                    where x2 = g x

                g Any = Any
                g (Val core typ a b) = Val core typ (h a) (if rec then h b else full)
                    where
                        rec = any (isFieldRecursive core) $ concat
                                [map (fromJust . snd) $ coreCtorFields $ coreCtor core c | (True,c) <- zip (valCtors a) ctrs]

                        full = let ValPart c d = b in ValPart (replicate (length c) True) (replicate (length d) Any)

                        dat = coreData core typ
                        flds = getFields core typ
                        ctrs = getCtors core typ

                        h (ValPart a b) = ValPart a [if c `elem` valid then d else Any | (c,d) <- zip flds b]
                            where
                                valid = concat [map (fromJust . snd) $ coreCtorFields c
                                               | (True,c) <- zip a (coreDataCtors dat)]

                isBlank (Val _ _ (ValPart a b) _) = all (== False) a || any isBlank b
                isBlank Any = False

                isComplete (Val _ _ a b) = comp a && comp b
                    where comp (ValPart a b) = all (== True) a && all isComplete b
                isComplete Any = True



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
                                        (zipWith mergeAnd (valFields $ valHead v) (valFields $ valTail v)))

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
