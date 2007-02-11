
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


data Val = Val {valType :: CoreData, valHead :: ValPart, valTail :: ValPart}
         | Any

data ValPart = ValPart {valCtors :: [Bool], valFields :: [Val]}
               deriving (Eq, Ord, Show)

type Vals = [Val]


representVal Any = (0, Nothing)
representVal (Val _ a b) = (1, Just (a,b))


instance Eq Val where
    a == b = representVal a == representVal b

instance Ord Val where
    compare x y = compare (representVal x) (representVal y)


instance Show Val where
    showList xs = showString $ "[" ++ concat (intersperse " | " $ map show xs) ++ "]"

    show Any = "_"

    show (Val dat hd tl) = showPart hd ++ (if completeValPart tl then "" else " * " ++ showPart tl)
        where
            ctrs = getCtors dat
            flds = getFields dat
        
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
subsetVal _ Any = True
subsetVal Any _ = False

subsetVal (Val _ a1 b1) (Val _ a2 b2) = subsetValPart a1 a2 && subsetValPart b1 b2


subsetValPart :: ValPart -> ValPart -> Bool
subsetValPart (ValPart a1 b1) (ValPart a2 b2)
    = and (zipWith (<=) a1 a2) &&
      and (zipWith subsetVal b1 b2)



completeVal :: Val -> Bool
completeVal Any = True
completeVal (Val _ a b) = completeValPart a && completeValPart b

completeValPart :: ValPart -> Bool
completeValPart (ValPart a b) = and a && all completeVal b


-- a `mergeAnd` b = c
-- c `subsetValue` a && c `subsetValue` b
mergeAnd :: Val -> Val -> Val
mergeAnd Any x = x
mergeAnd x Any = x
mergeAnd (Val dat a1 a2) (Val _ b1 b2) = Val dat (f a1 b1) (f a2 b2)
    where
        f (ValPart a1 a2) (ValPart b1 b2) = ValPart (zipWith (&&) a1 b1) (zipWith mergeAnd a2 b2)



-- a `strengthenVal` b = c
-- b `subset` c, c `subset` (a `union` b)
strengthenVal :: Val -> Val -> Val
strengthenVal Any x = Any
strengthenVal x Any = Any
strengthenVal (Val _ (ValPart ac af) ar) (Val dat (ValPart bc bf) br) = Val dat (ValPart bc2 bf2) br2
    where
        -- a `superset` b
        ac_bc = False -- and $ zipWith (<=) bc ac
        af_bf = and $ zipWith subsetVal bf af
        ar_br = False -- isNothing br || isNothing ar || subsetValPart (fromJust br) (fromJust ar)

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
normaliseVal (Val dat a b) = Val dat a b
{-
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
-}



normalise2 :: Core -> Vals -> Vals
normalise2 _ input = traceNone (show ("normalise",input,output)) output
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



normalise :: Core -> Vals -> Vals
normalise _ input | null input = []
                  | Any `elem` input = [Any]
                  | any completeVal output = [Any]
                  | otherwise = output
    where
        output = snub $ execute $ snub input
    
        dat = valType $ head input
        typ = getCtorsFields dat
        info = annotateSize 0 typ
        
        annotateSize n [] = []
        annotateSize n ((c,f):cf) = (n,fn) : annotateSize fn cf
            where fn = length f
        
        
        execute = concat . map executeTail . groupSortBy (compare `on` thd3) . expandItems
        
        expandItems :: [Val] -> [(Int, [Val], ValPart)]
        expandItems = concatMap f
            where
                f (Val _ (ValPart a b) c) = map (g b c) [i | (True,i) <- zip a [0..]]
                
                g b c i = (i, take count (drop start b), c)
                    where (start,count) = info !! i


        executeTail :: [(Int, [Val], ValPart)] -> [Val]
        executeTail = map merge . sequence . map executeMid . groupSortBy (compare `on` fst3)
        
        
        merge :: [Val] -> Val
        merge xs = foldr1 f xs
            where
                f (Val dat (ValPart a1 b1) c) (Val _ (ValPart a2 b2) _) =
                    Val dat (ValPart (zipWith (||) a1 a2) (zipWith g b1 b2)) c
                
                g Any x = x
                g x Any = x
        
        
        -- by this stage all fst3 and thd3 are the same
        executeMid :: [(Int, [Val], ValPart)] -> [Val]
        executeMid xs = concatMap (f . snd3) xs
            where
                (i,_,tl) = head xs
                (start,count) = info !! i
                ctr = replicate (length typ) False !!! (i, True)
                pre = replicate start Any
                post = replicate (length (concatMap snd typ) - start - count) Any
                
                f v | any isBlank v = []
                    | otherwise = [Val dat (ValPart ctr (pre ++ v ++ post)) tl]


        isBlank Any = False
        isBlank (Val _ (ValPart a _) _) = not $ or a



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
        [Val typ (ValPart (map (`elem` rest) ctrs) fields) (ValPart (replicate (length ctrs) True) fields)]
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
        
        f v | rec = Val dat
                       (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any))
                       (ValPart (zipWith (&&) (valCtors $ valHead v) (valCtors $ valTail v))
                                (zipWith mergeAnd (valFields $ valHead v) (valFields $ valTail v)))

            | otherwise = Val dat
                       (ValPart (replicate nctrs False !!! (ictr,True)) (replicate nflds Any !!! (ifld,v)))
                       (ValPart (replicate nctrs True) (replicate nflds Any))



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
            | isFieldRecursive core fld = Val typ cont cont
            | otherwise = vals !! fromJust (findIndex (== fld) flds)
