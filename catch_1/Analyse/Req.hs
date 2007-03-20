
module Analyse.Req(
    PropReq, Req(..), Constraint,
    propCon, conTrue, conAnd, conBool,
    notin, (|>), (<|),
    replaceVars
    ) where

import Yhc.Core
import Data.Proposition
import Data.List
import Data.Maybe
import General.General
import Analyse.Info
import Analyse.Factorise



---------------------------------------------------------------------
-- Req definition, properties on Req

type PropReq a = Formula (Req a)

data Req a = a :< Constraint
             deriving (Eq, Show, Ord)


instance (Show a, Ord a) => PropLit (Req a) where
    (e1 :< a) ?/\ (e2 :< b)
        | e1 == e2 = conReduce $ e1 :< conAnd a b
        | otherwise = None

    (e1 :< a) ?\/ (e2 :< b)
        | e1 == e2 = conReduce $ e1 :< conOr  a b
        | otherwise = None

    simp (e :< a) = conBool a

conReduce (e :< c) = maybe (Value $ e :< c) Literal (conBool c)


-- reduce a proposition to a single constraint
-- demand that each element is the given type
propCon :: (Show a, Ord a) => Info -> a -> PropReq a -> Constraint
propCon info x p = propFold (PropFold (conOrs info) (conAnds info) conNot conLit) p
    where
        conNot = error "Analyse.Req.propCon, no conNot exists"
        conLit (a :< b) | a /= x = error $ "Analyse.Req.propCon, expected " ++ show x ++ ", found: " ++ show p
                        | otherwise = b


replaceVars :: [CoreExpr] -> PropReq Int -> PropReq CoreExpr
replaceVars xs = propChange (\(i:<k) -> propLit $ (xs!!i) :< k)


---------------------------------------------------------------------
-- Type signature and instances

data Constraint = Con {conInfo :: Info, conVals :: [Val]}

instance Eq Constraint where
    (Con _ x) == (Con _ y) = x == y

instance Ord Constraint where
    compare (Con _ x) (Con _ y) = compare x y

instance Show Constraint where
    show (Con _ x) = showVals x


-- in any set of Matches, each constructor must occur at most once
-- and must be in alphabetical order
-- note: ctors always returns alphabetical order
data Val = Match :* [Match]
           deriving (Eq, Ord)

valFst (a :* b) = a
valSnd (a :* b) = b

data Match = Match {matchName :: CoreCtorName, matchVals :: [Val]}
           | Any
             deriving (Eq, Ord)


instance Show Val where
    show (Any :* []) = "_"
    show (a :* b) = "{" ++ show a ++ (if null b then "" else " * " ++ f b) ++ "}"
        where f = concat . intersperse "," . map show

showVals [] = "0"
showVals xs = concat $ intersperse " | " $ map show xs




instance Show Match where
    show (Match name vals) = unwords (name : map show vals)
    show Any = "_"


---------------------------------------------------------------------
-- Constraints as propositions

boolCon :: Info -> Bool -> Constraint
boolCon info x = (if x then conTrue else conFalse) info


anyVal = Any :* []

conTrue  info = Con info [anyVal]
conFalse info = Con info []


conBool :: Constraint -> Maybe Bool
conBool (Con _ c)
    | c == []       = Just False
    | c == [anyVal] = Just True
    | otherwise = Nothing


conOrs  info xs = if isComplete res then conTrue info else res
    where res = foldr conOr  (conFalse info) xs

conAnds info = foldr conAnd (conTrue  info)


conOr :: Constraint -> Constraint -> Constraint
conOr (Con info x) (Con _ y) = normalise $ makeComplete $ Con info $ normSubsets [x,y]


conAnd :: Constraint -> Constraint -> Constraint
conAnd (Con info x) (Con _ y) = normalise $ Con info $ normSubsets $ map (:[]) $
                                catMaybes [a `mergeVal` b | a <- x, b <- y]


---------------------------------------------------------------------
-- The standard operations and useful auxiliaries


-- useful auxiliaries, non recursive fields
nonRecs :: Info -> CoreCtorName -> [Int]
nonRecs info c = [i | i <- [0..arity info c - 1], not $ isRec info (c,i)]

hasRecs :: Info -> CoreCtorName -> Bool
hasRecs info c = any (isRec info . (,) c) [0..arity info c - 1]


-- a complete Match on |c|
completeVal :: Info -> CoreCtorName -> Val
completeVal info c = completeMatch info c :* [Any | hasRecs info c]

completeMatch :: Info -> CoreCtorName -> Match
completeMatch info c = Match c (map (const anyVal) (nonRecs info c))


notin :: Info -> [CoreCtorName] -> Constraint
notin info c = Con info $ map (completeVal info) (sort valid)
    where valid = ctors info (head c) \\ c


(|>) :: CoreField -> Constraint -> Constraint
(c,i) |> (Con info k) = res
    where
    res = normalise $ Con info $ conVals (notin info [c]) ++ concatMap f k
    
    rec = isRec info (c,i)
    
    f (Any :* []) = [anyVal]
    f (ms1 :* ms2) | rec = [res | not $ null ms2]
        where
          res = completeMatch info c :*
                mergeMatches ms2 (snub $ ms1 : [a | a :* b <- k, b `subsetMatches` ms2])

    f v = [Match c [if i == j then v else anyVal | j <- nonRecs info c] :*
           [Any | hasRecs info c]]


(<|) :: CoreCtorName -> Constraint -> PropReq Int
c <| (Con info vs) | anyVal `elem` vs = propTrue
                   | otherwise = propOrs $ map f res
    where
    (rec,non) = partition (isRec info . (,) c) [0..arity info c-1]
    
    res :: [([Match],[[Val]])]
    res = ungroupKey $ factors $ groupKey [(ms2, ms1) | Match c2 ms1 :* ms2 <- vs, c2 == c]
    
    factors xs = [(a, factorise anyVal b) | (a,b) <- xs]
    
    f (cont,now) = propAnds $ map propLit $
        (if Any `elem` cont then [] else map (:< Con info [m :* cont | m <- cont]) rec) ++
        (zipWith g non now)
    
    g n xs = n :< conOrs info (map (Con info . (:[])) xs)

{-
    = propOrs (map f vs)
    
    
    
    ans2 :: [([Match],[[Val]])]
    ans2 = groupBy ((==) `on` fst) $ sort (compare `on` fst) ans1

    f (Any :* ms2) = propTrue
    f (Match c2 ms1 :* ms2)
        | c2 == c = propAnds $ map propLit $
                               map (\n -> n :< Con info [ms1 !! n]) non ++
                               if Any `elem` ms2 then [] else
                                   map (:< Con info [m :* ms2 | m <- ms2]) rec
    f _ = propFalse
-}

---------------------------------------------------------------------
-- Merge items

-- a `merge` b = c
-- c `subset` a && c `subset` b
-- Nothing == False

mergeVal :: Val -> Val -> Maybe Val
mergeVal (Any :* []) x = Just x
mergeVal x (Any :* []) = Just x
mergeVal (a1 :* b1) (a2 :* b2) = do
    a3 <- mergeMatch a1 a2
    return $ a3 :* mergeMatches b1 b2


mergeMatch :: Match -> Match -> Maybe Match
mergeMatch Any x = Just x
mergeMatch x Any = Just x
mergeMatch (Match c1 vs1) (Match c2 vs2) | c1 == c2 && all isJust vs3 = Just $ Match c1 (map fromJust vs3)
    where vs3 = zipWith mergeVal vs1 vs2
mergeMatch _ _ = Nothing


mergeMatches :: [Match] -> [Match] -> [Match]
mergeMatches ms1 ms2 = catMaybes [mergeMatch a b | a <- ms1, b <- ms2]


---------------------------------------------------------------------
-- Subset items

subsetMatches :: [Match] -> [Match] -> Bool
subsetMatches as bs = all (\a -> any (\b -> a `subsetMatch` b) bs) as

subsetMatch :: Match -> Match -> Bool
subsetMatch _ Any = True
subsetMatch Any _ = False
subsetMatch (Match a as) (Match b bs) = a == b && and (zipWith subsetVal as bs)

subsetVal :: Val -> Val -> Bool
subsetVal (a1 :* b1) (a2 :* b2) = subsetMatch a1 a2 && subsetMatches b1 b2


---------------------------------------------------------------------
-- MultiPattern Normalise

normalise :: Constraint -> Constraint
normalise (Con info xs) = Con info $ snub xs



-- join two 
normSubsets :: [[Val]] -> [Val]
normSubsets xs = foldr f [] xs
    where
        f lhs rhs = g lhs rhs2 ++ g rhs2 lhs
            where rhs2 = rhs \\ lhs

        g xs ys = filter (\x -> not $ any (\y -> x `subsetVal` y) ys) xs



-- this could be done much better by combining makeComplete and isComplete
-- would make both faster and more general
makeComplete :: Constraint -> Constraint
makeComplete x@(Con info xs) | isComplete x = conTrue info
                             | otherwise = x


isComplete :: Constraint -> Bool
isComplete (Con info xs) = compList xs
    where
        compVal (Any :* x) = compSnd x
        compVal _ = False
    
        compList [] = False
        compList [Any :* x] = compSnd x
        compList xs = ctors info (head cs) == cs && all compCtor groups
            where
                groups = groupBy ((==) `on` (matchName . valFst)) $ snub xs
                cs = map (matchName . valFst . head) groups

        compCtor xs = all (compSnd . valSnd) xs && compRest (dropComp (map (matchVals . valFst) xs))

        dropComp ([]:_) = []
        dropComp xs = [x1 | not $ all compVal x1] ++ dropComp (map tail xs)
            where x1 = map head xs

        compRest [] = True
        compRest [x] = compList x
        compRest _ = False -- conservative
       
        compSnd [Any] = True
        compSnd [] = True
        compSnd _ =False


{-
    where
        res = Con info $ snub $ foldr add [] $ snub $ concatMap (valNorm info) xs
        
        add x [] = [x]
        add x xs | any (x `valSubsetEq`) xs = xs
        add x xs = old ++ new2
            where
                old = map (strengthen info x) xs
                new = map (\y -> strengthen info y x) xs
                new2 = filter (\n -> not $ any (\o -> n `valSubsetEq` o) old) new



-- should do as much one item normalisation as possible
-- currently very limited
valNorm :: Info -> Val -> [Val]
valNorm info Any = [Any]
valNorm info ([] :* _) = []
valNorm info x | isComps x = [Any]
               | otherwise = [x]
    where
        isComps (x :* y) = isComp x && maybe True isComp y
        isComps Any = True
    
        isComp [] = False
        isComp xs = map matchName xs == cs && all isComps (concatMap matchVals xs)
            where cs = ctors info (matchName $ head xs)


-- a \subseteq b
valSubsetEq _ Any = True
valSubsetEq Any _ = False
valSubsetEq (a1 :* a2) (b1 :* b2) = matchesSubsetEq a1 b1 &&
    (isNothing a2 || isNothing b2 || matchesSubsetEq (fromJust a2) (fromJust b2))


matchesSubsetEq :: [Match] -> [Match] -> Bool
matchesSubsetEq as bs = all (\a -> any (\b -> a `matchSubsetEq` b) bs) as

matchSubsetEq :: Match -> Match -> Bool
matchSubsetEq (Match a as) (Match b bs) = a == b && and (zipWith valSubsetEq as bs)


-- strengthen a b = c
-- b `subsetEq` c
-- a && b => c
strengthen :: Info -> Val -> Val -> Val
strengthen info Any _ = Any
strengthen info _ Any = Any
strengthen info (a1 :* b1) (a2 :* b2)
    | isNothing $ do c1 <- b1; c2 <- b2; if c1 == c2 then Nothing else Just ()
    = strengthenStart info a1 a2 :* listToMaybe (maybeToList b1 ++ maybeToList b2)
strengthen info a b = b


strengthenStart info a b = zipMatches f a b
    where
        f (Just a) Nothing  = a
        f Nothing  (Just b) = b
        f (Just a) (Just b) = b
-}
