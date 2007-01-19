
module Req(module PathCtor,
    Scopes(..), Scope(..), Reqs(..), Req(..),
    scopesAnds, newReq, newReqs,
    blurScopes, blurReqs
    ) where

import Yhc.Core
import General
import PathCtor
import Data.Proposition
import Data.List
import Data.Maybe
import Safe
import FixpProp


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope CoreFuncName Reqs

type Reqs = FixpProp Req

data Req = Req {reqExpr :: CoreExpr, reqPath :: PathCtor}
         | Demonic
         | Angelic
         deriving (Ord, Eq)


-- Formula Req has no negation within in
-- BDD Req may do

instance Show Scope where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req expr x) = showCoreExprGroup expr ++ show x
    show Demonic = "?Demonic"
    show Angelic = "?Angelic"

-- SMART CONSTRUCTORS

scopesAnds :: Scopes -> Scopes
scopesAnds xs = filter (\(Scope a b) -> not $ propIsTrue b) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]


newReq :: Core -> CoreExpr -> Path -> [CoreCtorName] -> Req
newReq core expr path ctors = Req expr (newPathCtorAlways core path ctors)

newReqs :: Core -> CoreExpr -> Path -> [CoreCtorName] -> Reqs
newReqs core expr path ctors =
    case newPathCtor core path ctors of
        Left b -> propBool b
        Right x -> propLit $ Req expr x


-- UTILITIES

instance PropLit Req where
    (?=>) = impliesReq
    (?/\) = combineReqsAnd
    (?\/) = combineReqsOr
    litNot = Just . notReq

-- SIMPLIFIERS

notReq Demonic = Demonic
notReq Angelic = Angelic
notReq (Req expr x) = Req expr (fromJust $ litNot x)


combineReqsAnd :: Req -> Req -> Reduce Req
combineReqsAnd (Req on1 pc1) (Req on2 pc2) =
    if on1 /= on2 then None else liftReduce (Req on1) (pc1 ?/\ pc2)


combineReqsOr :: Req -> Req -> Reduce Req
combineReqsOr (Req on1 pc1) (Req on2 pc2) =
    if on1 /= on2 then None else liftReduce (Req on1) (pc1 ?\/ pc2)


liftReduce :: (a -> b) -> Reduce a -> Reduce b
liftReduce f (Value x) = Value $ f x
liftReduce f (Literal x) = Literal x
liftReduce f None = None


impliesReq :: [(Req, Bool)] -> Req -> Maybe Bool
impliesReq given (Req on pc) =
        [(b,c) | (Req a b, c) <- given, a == on] ?=> pc


-- NEGATION AND BLURRING

blurReq :: Req -> Req
blurReq (Req expr (PathCtor hill path ctrs)) = newReq hill expr (blurPath hill path) ctrs

blurReqs :: Reqs -> Reqs
blurReqs = propMap f
    where
        f Demonic = propFalse
        f Angelic = propTrue
        f x = propLit $ blurReq x

blurScope :: Scope -> Scope
blurScope (Scope a b) = Scope a (blurReqs b)

blurScopes = map blurScope
