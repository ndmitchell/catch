
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



type PropReq a = Formula (Req a)


---------------------------------------------------------------------
-- Req definition

data Req a = a :< Constraint
             deriving (Eq, Show, Ord)


instance (Show a, Ord a) => PropLit (Req a) where
    (e1 :< a) ?/\ (e2 :< b)
        | e1 == e2 = conReduce $ e1 :< conAnd a b
        | otherwise = None

    (e1 :< a) ?\/ (e2 :< b)
        | e1 == e2 = conReduce $ e1 :< conOr  a b
        | otherwise = None


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
-- MultiPattern Constraint System

data Constraint = Con {conInfo :: Info, conVals :: [Val]}

instance Eq Constraint where
    (Con _ x) == (Con _ y) = x == y

instance Ord Constraint where
    compare (Con _ x) (Con _ y) = compare x y

instance Show Constraint where
    show (Con _ x) = show x


-- in any set of Matches, each constructor must occur at most once
-- and must be in alphabetical order
-- note: ctors always returns alphabetical order
data Val = [Match] :* (Maybe [Match])
         | Any
           deriving (Eq, Ord)

data Match = Match {matchName :: CoreCtorName, matchVals :: [Val]}
             deriving (Eq, Ord)


instance Show Val where
    showList xs = showString $ concat $ intersperse " | " $ map show xs

    show Any = "_"
    show (a :* b) = "{" ++ f a ++ " * " ++ maybe "_" f b ++ "}"
        where
            f [] = "0"
            f xs = concat $ intersperse "," $ map show xs

instance Show Match where
    show (Match name vals) = unwords (name : map show vals)



boolCon :: Info -> Bool -> Constraint
boolCon info x = (if x then conTrue else conFalse) info


conTrue  info = Con info [Any]
conFalse info = Con info []


conBool :: Constraint -> Maybe Bool
conBool (Con _ c)
    | null c       = Just False
    | Any `elem` c = Just True
    | otherwise    = Nothing


-- useful auxiliaries, non recursive fields
nonRecs :: Info -> CoreCtorName -> [Int]
nonRecs info c = [i | i <- [0..arity info c - 1], not $ isRec info (c,i)]

hasRecs :: Info -> CoreCtorName -> Bool
hasRecs info c = any (isRec info . (,) c) [0..arity info c - 1]

fillTail :: Info -> [Match] -> Val
fillTail info x = x :* if rec then Just tl else Nothing
    where
        tl = map (complete info) (ctors info (head cs))
        cs = [c | Match c _ <- x]
        rec = any (hasRecs info) cs


-- a complete Match on |c|
complete :: Info -> CoreCtorName -> Match
complete info c = Match c (map (const Any) (nonRecs info c))


notin :: Info -> [CoreCtorName] -> Constraint
notin info c = Con info [fillTail info $ map (complete info) $ sort valid]
    where
        valid = ctors info (head c) \\ c


(|>) :: CoreField -> Constraint -> Constraint
(|>) (c,i) (Con info k) = normalise $ Con info $ conVals (notin info [c]) ++ map f k
    where
    rec = isRec info (c,i)
    
    f Any = Any
    f (ms1 :* ms2) | rec = [complete info c] :* (Just $ maybe ms1 (merge ms1) ms2)
    f v =  fillTail info [Match c [if i == j then v else Any | j <- nonRecs info c]]


(<|) :: CoreCtorName -> Constraint -> PropReq Int
(<|) c (Con info vs) = propOrs (map f vs)
    where
    (rec,non) = partition (isRec info . (,) c) [0..arity info c-1]

    f Any = propTrue
    f (ms1 :* ms2) = propOrs [g vs | Match c1 vs1 <- ms1, c1 == c]
        where
            g :: [Val] -> PropReq Int
            g vs = propAnds $ map propLit $
                        zipWith (:<) non (map (Con info . (:[])) vs) ++
                        map (:< Con info [fromJust ms2 :* ms2]) rec


mergeVal :: Val -> Val -> Val
(a1 :* b1) `mergeVal` (a2 :* b2) = merge a1 a2 :* f b1 b2
    where f x y = do x2 <- x; y2 <- y; return $ merge x2 y2
Any `mergeVal` b = b
a `mergeVal` Any = a



zipMatches :: (Maybe Match -> Maybe Match -> a) -> [Match] -> [Match] -> [a]
zipMatches f [] xs = map (     f Nothing . Just) xs
zipMatches f ys [] = map (flip f Nothing . Just) ys
zipMatches f (x:xs) (y:ys) = case compare (matchName x) (matchName y) of
    EQ -> f (Just x) (Just y) : zipMatches f xs ys
    LT -> f (Just x) Nothing  : zipMatches f xs (y:ys)
    GT -> f Nothing  (Just y) : zipMatches f (x:xs) ys


merge :: [Match] -> [Match] -> [Match]
merge ms1 ms2 = catMaybes $ zipMatches f ms1 ms2
    where
        f x y = do
            Match c1 vs1 <- x
            Match c2 vs2 <- y
            return $ Match c1 (zipWith mergeVal vs1 vs2)


conOrs  info = foldr conOr  (conFalse info)
conAnds info = foldr conAnd (conTrue  info)


conOr :: Constraint -> Constraint -> Constraint
conOr (Con info x) (Con _ y) = normalise $ Con info $ x ++ y


conAnd :: Constraint -> Constraint -> Constraint
conAnd (Con info x) (Con _ y) = normalise $ Con info [a `mergeVal` b | a <- x, b <- y]



---------------------------------------------------------------------
-- MultiPattern Normalise

normalise :: Constraint -> Constraint
normalise (Con info xs) = res
    where
        res = Con info $ snub $ foldr add [] $ snub $ concatMap (valNorm info) xs
        
        add x [] = [x]
        add x xs = old ++ new2
            where
                old = map (strengthen info x) xs
                new = map (\y -> strengthen info y x) xs
                new2 = filter (\n -> not $ any (\o -> n `valSubsetEq` o) old) new



-- should do as much one item normalisation as possible
-- currently very limited
valNorm :: Info -> Val -> [Val]
valNorm info ([] :* _) = []
valNorm info x = [x]


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
