{-# OPTIONS_GHC -fallow-undecidable-instances #-}
-- for the instance of Formula p

module Tram.Req(module Tram.Req, module Tram.Path) where

import Hill.All
import General.General
import Tram.Path
import Data.Proposition
import Data.List
import Data.Maybe
import Safe


-- DATA DEFINITIONS

type Scopes p = [Scope p]
data Scope p = Scope FuncName (p Req)


data Req = Req Hill Expr Path [CtorName]
         | Demonic
         | Angelic


reqExpr (Req _ x _ _) = x


-- Formula Req has no negation within in
-- BDD Req may do

instance Eq Req where
    (Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2
    Demonic == Demonic = True
    Angelic == Angelic = True
    _ == _ = False

instance Ord Req where
    compare (Req _ a1 b1 c1) (Req _ a2 b2 c2) = compare (a1,b1,c1) (a2,b2,c2)
    compare (Req{}) _ = GT
    compare _ (Req{}) = LT
    compare x y = compare (x==Demonic, x==Angelic) (y==Demonic, y==Angelic)

instance Show (p Req) => Show (Scope p) where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req _ expr path ctor) =
        showExprBrackets expr ++ show path ++ strSet ctor
    show Demonic = "?Demonic"
    show Angelic = "?Angelic"


-- SMART CONSTRUCTORS

scopesAnds :: Prop p => Scopes p -> Scopes p
scopesAnds xs = filter (\(Scope a b) -> not (propIsTrue b)) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]




newReq :: Hill -> Expr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors

{-
x.tl*{[]} => x{[]}
x.p{c} | ewp(p), and x{c} => no items available in p
-}
    | ewpPath path
    = Req hite zexpr (restrictPath path $ concatMap (ctorArgs . getCtor hite) ctors) (snub ctors)
    
    
    | otherwise = Req hite zexpr path (snub ctors)



newReqs :: Prop p => Hill -> Expr -> Path -> [CtorName] -> p Req
newReqs hite zexpr path ctors | null ctors = propFalse
                              | ctors `setEq` baseSet = propTrue
                              | otherwise = propLit $ newReq hite zexpr path ctors
    where
        baseSet = ctorNames $ getCtor hite (headNote "Tram.Type.impliesReq here" ctors)


-- UTILITIES

instance PropLit Req where
    (?=>) = impliesReq
    (?/\) = combineReqsAnd
    (?\/) = combineReqsOr
    litNot = Just . notReq


-- SIMPLIFIERS

notReq Demonic = Demonic
notReq Angelic = Angelic
notReq (Req hill expr path ctrs) = newReq hill expr path ctrs2
    where ctrs2 = sort $ ctorNames (getCtor hill (head ctrs)) \\ ctrs

combineReqsAnd :: Req -> Req -> Reduce Req
combineReqsAnd (Req hite on1 path1 ctors1) (Req _ on2 path2 ctors2)
        | on1 == on2 && path1 == path2
        = if null ctrs then Literal False else Value (newReq hite on1 path1 ctrs)
    where
        ctrs = sort $ ctors2 `intersect` ctors1

combineReqsAnd r1 r2
        | isJust s1 || isJust s2
        = fromMaybe (combineReqsAnd t1 t2) (reduceAndWithImp t1 t2)
    where
        (s1,s2) = (reduceAnd r2 r1, reduceAnd r1 r2)
        (t1,t2) = (fromMaybe r1 s1, fromMaybe r2 s2)

combineReqsAnd _ _ = None


-- given that a predicate is anded, what must this one be
-- must make things smaller, or returns Nothing
reduceAnd :: Req -> Req -> Maybe Req
reduceAnd (Req hite on1 path1 ctors1) (Req _ on2 path2 ctors2)
    | on1 == on2 && path2 `subsetPath` path1 && ctors2 /= ctrs
    = Just $ Req hite on2 path2 ctrs
    where ctrs = ctors1 `intersect` ctors2
reduceAnd _ x = Nothing


combineReqsOr :: Req -> Req -> Reduce Req
combineReqsOr (Req hite on1 path1 ctors1) (Req _ on2 path2 ctors2)
        | on1 == on2 && path1 == path2 && finitePath path1
        = if length ctrs == length baseSet then Literal True else Value (newReq hite on1 path1 ctrs)
    where
        ctrs = snub $ ctors2 ++ ctors1
        baseSet = ctorNames $ getCtor hite (head ctrs)
        
combineReqsOr _ _ = None


-- impliesPair a b, a => b
impliesPair :: Req -> Req -> Bool
impliesPair r1@(Req hite on path ctors) r2@(Req _ on2 path2 ctors2)
    | on == on2 && ctors2 `subset` ctors && newReq hite on path ctors2 == r2 = True
    | on == on2 && ctors `subset` ctors2 && path2 `subsetPath` path = True
impliesPair _ _ = False




impliesReq :: [(Req, Bool)] -> Req -> Maybe Bool
impliesReq given req@(Req hite on path ctors) =
        if null ctors then Just False
        else if any doesImply given then Just True
        else if poss `subset` ctors then Just True
        else if ctors `disjoint` poss then Just False
        else Nothing
    where
        doesImply :: (Req,Bool) -> Bool
        doesImply (r2,b) = b && impliesPair req r2

        -- calculate all possible constructors that might arise
        poss = foldr f baseSet given
        baseSet = ctorNames $ getCtor hite (headNote "Tram.Type.impliesReq" ctors)
        
        f (Req _ on2 path2 ctors2, False) poss
            | on2 == on && path2 == path && finitePath path
            = poss \\ ctors2
        
        f (Req _ on2 path2 ctors2,True) poss
            | on2 == on && path `subsetPath` path2
            = poss `intersect` ctors2

        f _ poss = poss
        
impliesReq _ _ = Nothing


-- NEGATION AND BLURRING

blurReq :: Req -> Req
blurReq (Req hill expr path ctrs) = newReq hill expr (blurPath hill path) ctrs

blurReqs :: Formula Req -> Formula Req
blurReqs = propMap f
    where
        f Demonic = propFalse
        f Angelic = propTrue
        f x = propLit $ blurReq x

blurScope :: Scope Formula -> Scope Formula
blurScope (Scope a b) = Scope a (blurReqs b)

blurScopes = map blurScope
