
module Tram.Req(module Tram.Req, module Tram.Path) where

import Hill.All
import General.General
import Tram.Path
import Data.Proposition
import Data.List
import Safe


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope FuncName (BDD Req)
             deriving (Eq, Ord)

data Req = Req Hill Expr Path [CtorName]

-- Formula Req has no negation within in
-- BDD Req may do

instance Eq Req where
    (Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

instance Ord Req where
    compare (Req _ a1 b1 c1) (Req _ a2 b2 c2) = compare (a1,b1,c1) (a2,b2,c2)

instance Show Scope where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req _ expr path ctor) =
        showExprBrackets expr ++ show path ++ strSet ctor


-- SMART CONSTRUCTORS

scopesAnds :: Scopes -> Scopes
scopesAnds xs = filter (\(Scope a b) -> not (propIsTrue b)) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]




newReq :: Hill -> Expr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors
    | path == newPath hite ["tl"] && ctors == ["[]"] = Req hite zexpr (emptyPath hite) ctors
    | otherwise = Req hite zexpr path (nub $ sort ctors)

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
    litNot = Just . notReq


-- SIMPLIFIERS

combineReqsAnd :: Req -> Req -> Reduce Req
combineReqsAnd (Req hite on1 path1 ctors1) (Req _ on2 path2 ctors2)
    | on1 == on2 && path1 == path2 = Value (Req hite on1 path1 (sort $ ctors2 `intersect` ctors1))
    | otherwise = None


impliesReq :: [(Req, Bool)] -> Req -> Maybe Bool
impliesReq given req@(Req hite on path ctors) = 
        if null ctors then Just False
        else if poss `subset` ctors then Just True
        else if ctors `disjoint` poss then Just False
        else Nothing
    where
        baseSet = ctorNames $ getCtor hite (headNote "Tram.Type.impliesReq" ctors)
        poss = foldr f baseSet given
        
        f (Req _ on2 path2 ctors2, False) poss
            | on2 == on && path2 == path && finitePath path
            = poss \\ ctors2
        
        f (Req _ on2 path2 ctors2,True) poss
            | on2 == on && path `subsetPath` path2
            = poss `intersect` ctors2

        f _ poss = poss
        
impliesReq _ _ = Nothing


-- NEGATION AND BLURRING

notReq (Req hill expr path ctrs) = Req hill expr path ctrs2
    where ctrs2 = ctorNames (getCtor hill (head ctrs)) \\ ctrs


blurReqs :: Formula Req -> Formula Req
blurReqs = propMap f
    where f (Req hill expr path ctrs) = propLit $ Req hill expr (blurPath hill path) ctrs
