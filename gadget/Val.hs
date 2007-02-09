
module Val(
    Val(..), Vals,
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


---------------------------------------------------------------------
-- DEBUGGING DATA STRUCTURES

listCoreData = CoreData "[]" [] [CoreCtor ":" [("",Just "hd"), ("",Just "tl")], CoreCtor "[]" []]

listCore = Core [] [] [listCoreData] []


---------------------------------------------------------------------
-- UTILITIES FOR EXTRACTING INFORMATION

getCtors :: CoreData -> [CoreCtorName]
getCtors = map fst . getCtorsFields


getFields :: CoreData -> [CoreFieldName]
getFields = concatMap snd . getCtorsFields


prepType :: String -> String
prepType = filter (`notElem` "()")


getCtorsFields :: CoreData -> [(CoreCtorName, [CoreFieldName])]
getCtorsFields dat = [(coreCtorName ctr, concatMap f $ coreCtorFields ctr) | ctr <- coreDataCtors dat]
    where
        f (typ, Just field) = [field | prepType typ /= rec]
        rec = unwords $ coreDataName dat : coreDataTypes dat



-- those constructors which have a field which is recursive
getCtorsRec :: CoreData -> [CoreCtorName]
getCtorsRec dat = [coreCtorName ctr | ctr <- coreDataCtors dat, any ((== rec) . prepType . fst) (coreCtorFields ctr)]
    where rec = unwords $ coreDataName dat : coreDataTypes dat



---------------------------------------------------------------------
-- DATA VALUES

-- If a valTail is Nothing then no recursive constructors are present
-- If a value is Void then the constructor for the field is not allowed


data Val = Val {valType :: CoreData, valHead :: ValPart, valTail :: Maybe ValPart}
         | Any
         | Void

data ValPart = ValPart {valCtors :: [Bool], valFields :: [Val]}
               deriving (Eq, Ord, Show)

type Vals = [Val]


representVal Any = (0, Nothing)
representVal Void = (1, Nothing)
representVal (Val _ a b) = (2, Just (a,b))


instance Eq Val where
    a == b = representVal a == representVal b

instance Ord Val where
    compare x y = compare (representVal x) (representVal y)


instance Show Val where
    showList xs = showString $ "[" ++ concat (intersperse " | " $ map show xs) ++ "]"

    show Any = "_"
    show Void = "0"
    
    show (Val dat hd tl) =
            showPart hd ++
            case tl of {Just x | not $ complete x -> " * " ++ showPart x; _ -> ""}
        where
            ctrs = getCtors dat
            flds = getFields dat
        
            complete (ValPart ctrs fields) = and ctrs && all (== Any) fields
            
            showPart (ValPart ctrs fields) = showCtrs ctrs ++
                if all (== Any) fields then ""
                else if length ctrs == 1 || length fields <= 1 then showFields fields
                else showFieldsRec fields
        
            showCtrs xs | null res = "0"
                        | otherwise = concat $ intersperse "+" res
                where res = [c | (True,c) <- zip xs ctrs]
    
            showFields xs = concatMap (\x -> " {" ++ show x ++ "}") xs
    
            showFieldsRec xs = " {" ++ concat (intersperse ", " $ zipWith f xs flds) ++ "}"
                where f x name = name ++ "=" ++ show x


---------------------------------------------------------------------
-- LOW LEVEL UTILITIES FOR COMBINING VAL'S

-- is a `subset` b || a == b
subsetVal :: Val -> Val -> Bool
subsetVal Void _ = True
subsetVal _ Void = True
subsetVal _ Any = True
subsetVal Any _ = False

subsetVal (Val _ a1 b1) (Val _ a2 b2) = subsetValPart a1 a2 && fMaybe b1 b2
    where
        fMaybe (Just x) (Just y) = subsetValPart x y
        fMaybe _ _ = True


subsetValPart :: ValPart -> ValPart -> Bool
subsetValPart (ValPart a1 b1) (ValPart a2 b2)
    = and (zipWith (<=) a1 a2) &&
      and (zipWith subsetVal b1 b2)


equalVal :: Val -> Val -> Bool
equalVal Void _ = True
equalVal _ Void = True
equalVal (Val _ a1 b1) (Val _ a2 b2) = equalValPart a1 a2 && rest
    where rest = isNothing b1 || isNothing b2 || equalValPart (fromJust b1) (fromJust b2)
equalVal x y = x == y


equalValPart :: ValPart -> ValPart -> Bool
equalValPart (ValPart a1 b1) (ValPart a2 b2) = a1 == a2 && and (zipWith equalVal b1 b2)


completeVal :: Val -> Bool
completeVal Any = True
completeVal Void = False
completeVal (Val _ a b) = completeValPart a && maybe True completeValPart b

completeValPart :: ValPart -> Bool
completeValPart (ValPart a b) = and a && all (\x -> x == Void || completeVal x) b


-- a `mergeAnd` b = c
-- c `subsetValue` a && c `subsetValue` b
mergeAnd :: Val -> Val -> Val
mergeAnd Void x = x
mergeAnd x Void = x
mergeAnd Any x = x
mergeAnd x Any = x
mergeAnd (Val dat a1 a2) (Val _ b1 b2) = Val dat (f a1 b1) (fMaybe a2 b2)
    where
        fMaybe (Just x) (Just y) = Just $ f x y
        fMaybe Nothing x = x
        fMaybe x Nothing = x
        fMaybe _ _ = Nothing
    
        f (ValPart a1 a2) (ValPart b1 b2) = ValPart (zipWith (&&) a1 b1) (zipWith mergeAnd a2 b2)



-- a `strengthenVal` b = c
-- b `subset` c, c `subset` (a `union` b)
strengthenVal :: Val -> Val -> Val
strengthenVal Void x = x
strengthenVal x Void = x
strengthenVal Any x = Any
strengthenVal x Any = Any
strengthenVal (Val _ (ValPart ac af) ar) (Val dat (ValPart bc bf) br) = Val dat (ValPart bc2 bf2) br2
    where
        -- a `superset` b
        ac_bc = False -- and $ zipWith (<=) bc ac
        af_bf = and $ zipWith subsetVal bf af
        ar_br = isNothing br || isNothing ar || subsetValPart (fromJust br) (fromJust ar)

        bc2 = if af_bf && ar_br then zipWith (||) ac bc else bc
        bf2 = if ac_bc && ar_br then zipWith strengthenVal af bf else bf
        br2 = if ac_bc && af_bf then br else br


-- normalise a value
-- if all values are possible, return Any
-- if no valid values are possible, return Void
-- if a child is unimportant, return Void
-- reduce each rule in isolation
normaliseVal :: Val -> Val
normaliseVal Any = Any
normaliseVal Void = Void
normaliseVal (Val dat a b) =
        case b >>= normalisePart of
            Nothing ->
                let ValPart a1 a2 = a
                    aNew = ValPart (zipWith (\x1 x2 -> x1 && not x2) a1 recc) a2
                in case normalisePart aNew of
                    Nothing -> Void
                    Just a2 | completeValPart a2 -> Any
                            | otherwise -> Val dat a2 Nothing

            Just b2 -> case normalisePart a of
                Nothing -> Void
                Just an@(ValPart a1 a2)
                    | or (zipWith (&&) a1 recc) -> checkComplete $ Val dat an (Just b2)
                    | otherwise -> checkComplete $ Val dat an Nothing
    where
        checkComplete x = if completeVal x then Any else x
    
        ctrr = getCtorsRec dat
        ctfd = getCtorsFields dat
        ctrs = map fst ctfd
        flds = concatMap snd ctfd
        
        -- those constructors which have a recursive element
        recc = map (`elem` ctrr) ctrs

    
        -- void out all the fields which can't exist
        -- if any field is void, void out the constructor
        -- if whole thing ends up as void, return Nothing
        normalisePart (ValPart a b) = if or a2 then Just (ValPart a2 b3) else Nothing
            where
                b3 = voidFields a2 b2 ctfd
                a2 = voidCtors a b2 ctfd
                b2 = map normaliseVal b
                
                -- if a constructor is marked as valid, but has a void field
                -- set it to invalid
                voidCtors _ _ [] = []
                voidCtors (a:as) bs ((c,f):cf) = (a && all (/= Void) bsNow) : voidCtors as bsRest cf
                    where (bsNow, bsRest) = splitAt (length f) bs

                voidFields _ _ [] = []
                voidFields (a:as) bs ((c,f):cf) = bsNew ++ voidFields as bs cf
                    where
                        bsNew = if a then bsNow else replicate (length bsNow) Void
                        (bsNow, bsRest) = splitAt (length f) bs




normalise :: Core -> Vals -> Vals
normalise _ input = traceNone (show ("normalise",input,output)) output
    where
        output = process input
        process = snub . ruleCombine . snub . map normaliseVal . snub


ruleCombine :: [Val] -> [Val]
ruleCombine xs = f [] xs
    where
        f done [] = done
        f done (t:odo) = f done2 (todo2++odo)
            where (done2,todo2) = g t done

        
        -- given a comparison element
        -- and a list of things which have been done
        -- give back a list of things still done, and things to do
        g :: Val -> [Val] -> ([Val],[Val])
        g t [] = ([t],[])
        g t (x:xs) | t `subsetVal` v = (xs, [v])
                   | v `subsetVal` t = (xs, [t])
                   | v == x = (v:a, b)
                   | otherwise = (a, v:b)
            where
                v = normaliseVal $ strengthenVal t x
                (a,b) = g t xs


{-
---------------------------------------------------------------------
-- NORMALISE

-- properties of normalise, with X as the result:
--
-- \forall x, y \in X, x \not-subset y
-- if x is a member, it must not be a subset of any other members
--
-- \forall x \in X, y \in \Universe, x \subset y => y \not-subset-eq X
-- if y is a superset of x and is valid in X, then x should not be present
--
normalise :: Core -> Vals -> Vals
normalise _ = snub . ruleSubset . ruleCombine . ruleIntroduce . ruleIndividual . snub

-- BEGIN
-- NORMALISE - Functions lifted for better profiling

-- combine pairs into one
ruleCombine :: [Val] -> [Val]
ruleCombine vals = f [] vals
    where
        f done [] = done
        f done (t:odo) = case g t done of
                            Nothing -> f (t:done) odo
                            Just (new,done2) -> f done2 (new:odo)

        g :: Val -> [Val] -> Maybe (Val, [Val])
        g y xs = do
                ind <- findIndex isJust res
                return (fromJust (res !! ind), (xs \!! ind))
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
ruleIntroduce xs | Any `elem` xs = [Any]
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


-- END NORMALISE

-}

---------------------------------------------------------------------
-- PROPOSITIONAL COMBINATIONS FOR VAL'S


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



---------------------------------------------------------------------
-- UTILITIES FOR REDUCE


-- is the root allowed
checkRoot :: Core -> Vals -> CoreCtorName -> Bool
checkRoot core vals name = Any `elem` vals || any f vals
    where
        typ = valType $ head vals
        ind = fromJust $ findIndex (== name) $ getCtors typ
        f val = valCtors (valHead val) !! ind


-- some simple constructors
anyCtor :: Core -> [CoreCtorName] -> Vals
anyCtor core [] = []
anyCtor core rest = normalise core
        [Val typ (ValPart (map (`elem` rest) ctrs) fields) (Just $ ValPart (replicate (length ctrs) True) fields)]
    where
        typ = coreCtorData core (head rest)
        ctrs = getCtors typ
        fields = replicate (length $ getFields typ) Any


integrate :: Core -> Vals -> CoreFieldName -> Vals
integrate core vals field
        | Any `elem` vals = [Any]
        | otherwise = normalise core $ anyCtor core (delete name $ getCtors dat) ++ map f vals
    where
        dat = coreCtorData core name
        ctr@(CoreCtor name fields) = coreFieldCtor core field
        rec = isFieldRecursive core field
        
        ctrs = coreDataCtors dat
        flds = getFields dat
        
        (nctrs,nflds) = (length ctrs, length flds)
        
        ictr = fromJust $ findIndex ((== name) . coreCtorName) ctrs
        ifld = fromJust $ findIndex (== field) flds
        
        f v | rec = let part = fromMaybe anyPart (valTail v) in
                    Val dat
                       (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any))
                       (Just $ ValPart (zipWith (&&) (valCtors $ valHead v) (valCtors part))
                                       (zipWith mergeAnd (valFields $ valHead v) (valFields part)))

            | otherwise = Val dat
                       (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any !!! (ifld,v)))
                       (Just anyPart)

        anyPart = ValPart (replicate nctrs True) (replicate nflds Any)




differentiate :: Core -> CoreCtorName -> Vals -> [[Val]]
differentiate core name vals
        | null vals = []
        | Any `elem` vals = [replicate (length $ coreCtorFields ctor) Any]
        | otherwise = [map (f (v,part) . fromJust . snd) $ coreCtorFields ctor 
                      | Val _ (ValPart c v) part <- vals, c !! ictr]
    where
        typ = valType $ head vals
        ctrs = getCtors typ
        flds = getFields typ
        ctor = coreCtor core name
        ictr = fromJust $ findIndex (==name) ctrs
        
        f (vals,cont) fld
            | isFieldRecursive core fld = Val typ (fromJust cont) cont
            | otherwise = vals !! fromJust (findIndex (== fld) flds)
