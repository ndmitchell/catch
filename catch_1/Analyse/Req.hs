
module Analyse.Req(
    PropReq, Req(..), Constraint,
    propCon, conTrue, conAnd,
    notin, (|>), (<|),
    replaceVars
    ) where

import Yhc.Core
import Data.Proposition
import Data.List
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
propCon :: (Show a, Ord a) => a -> PropReq a -> Constraint
propCon x p = propFold (PropFold conOrs conAnds conNot conLit) p
    where
        conNot = error "Analyse.Req.propCon, no conNot exists"
        conLit (a :< b) | a /= x = error "Analyse.Req.propCon, precondition failed"
                        | otherwise = b


replaceVars :: [CoreExpr] -> PropReq Int -> PropReq CoreExpr
replaceVars xs = propChange (\(i:<k) -> propLit $ (xs!!i) :< k)

                        
---------------------------------------------------------------------
-- MultiPattern Constraint System

type Constraint = [Val]

data Val = [Match] :* [Match]
         | Any
           deriving (Eq, Ord, Show)

data Match = Match CoreCtorName [Val]
             deriving (Eq, Ord, Show)


boolCon :: Bool -> Constraint
boolCon x = if x then conTrue else conFalse


conTrue  = [Any]
conFalse = []


conBool :: Constraint -> Maybe Bool
conBool c
    | null c       = Just False
    | Any `elem` c = Just True
    | otherwise    = Nothing


-- useful auxiliaries, non recursive fields
nonRecs :: Info -> CoreCtorName -> [Int]
nonRecs info c = [i | i <- [0..arity info c - 1], not $ isRec info (c,i)]


-- a complete Match on |c|
complete :: Info -> CoreCtorName -> Match
complete info c = Match c (map (const Any) (nonRecs info c))


notin :: Info -> CoreCtorName -> Constraint
notin info c = [map (complete info) (delete c cs) :* map (complete info) cs]
    where cs = ctors info c


(|>) :: CoreField -> Constraint -> Info -> Constraint
(|>) (c,i) k info = normalise $ notin info c ++ map f k
    where
    rec = isRec info (c,i)
    
    f Any = Any
    f (ms1 :* ms2) | rec = [complete info c] :* merge ms1 ms2
    f v =  [Match c [if i == j then v else Any | j <- nonRecs info c]]
           :* map (complete info) (ctors info c)


(<|) :: CoreCtorName -> Constraint -> Info -> PropReq Int
(<|) c vs info = propOrs (map f vs)
    where
    (rec,non) = partition (isRec info . (,) c) [0..arity info c-1]

    f Any = propTrue
    f (ms1 :* ms2) = propOrs [g vs | Match c1 vs1 <- ms1, c1 == c]
        where
            g :: [Val] -> PropReq Int
            g vs = propAnds $ map propLit $
                        zipWith (:<) non (map (:[]) vs) ++
                        map (:< [ms2 :* ms2]) rec


mergeVal :: Val -> Val -> Val
(a1 :* b1) `mergeVal` (a2 :* b2) = merge a1 a2 :* merge b1 b2


merge :: [Match] -> [Match] -> [Match]
merge  ms1 ms2 = [Match c1 (zipWith mergeVal vs1 vs2) |
       Match c1 vs1 <- ms1, Match c2 vs2 <- ms2, c1 == c2]



conOrs  = foldr conOr  conFalse
conAnds = foldr conAnd conTrue


conOr :: Constraint -> Constraint -> Constraint
conOr x y = normalise $ x ++ y


conAnd :: Constraint -> Constraint -> Constraint
conAnd x y = normalise [a `mergeVal` b | a <- x, b <- y]


---------------------------------------------------------------------
-- MultiPattern Normalise

normalise :: Constraint -> Constraint
normalise = id


