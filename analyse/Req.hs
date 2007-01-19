
module Req(module Req, module Path) where

import Yhc.Core
import General
import Path
import Data.Proposition
import Data.List
import Data.Maybe
import Safe
import FixpProp


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope CoreFuncName Reqs

type Reqs = FixpProp Req

data Req = Req CoreExpr PathCtor
         | Demonic
         | Angelic
         deriving (Ord, Eq)

data PathCtor = PathCtor Core Path [CoreCtorName]

type BoolPathCtor = Either Bool PathCtor

reqExpr (Req x _) = x



-- Formula Req has no negation within in
-- BDD Req may do

instance Eq PathCtor where
    (PathCtor _ a1 b1) == (PathCtor _ a2 b2) = a1 == a2 && b1 == b2

instance Ord PathCtor where
    compare (PathCtor _ a1 b1) (PathCtor _ a2 b2) = compare (a1,b1) (a2,b2)

instance Show Scope where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req expr x) = showCoreExprGroup expr ++ show x
    show Demonic = "?Demonic"
    show Angelic = "?Angelic"

instance Show PathCtor where
    show (PathCtor _ path ctor) = show path ++ strSet ctor

-- SMART CONSTRUCTORS

scopesAnds :: Scopes -> Scopes
scopesAnds xs = filter (\(Scope a b) -> not $ propIsTrue b) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]



newPathCtor :: Core -> Path -> [CoreCtorName] -> BoolPathCtor
newPathCtor core path ctors
    | null ctors = Left False -- NOTE: Not true! (conservative)
    | ctors `setEq` baseSet = Left True

    -- x.tl*{[]} => x{[]}
    -- x.p{c} | ewp(p), and x{c} => no items available in p
    | ewpPath path
      = let newpath = restrictPath path $ concatMap (map (fromJust . snd) . coreCtorFields . coreCtor core) ctors
        in Right $ PathCtor core newpath sctors

    | otherwise = Right $ PathCtor core path sctors
    where
        sctors = snub ctors
        baseSet = ctorNames $ coreCtorData core (headNote "Tram.Type.impliesReq here" ctors)

newPathCtorAlways :: Core -> Path -> [CoreCtorName] -> PathCtor
newPathCtorAlways core path ctors =
    case newPathCtor core path ctors of
        Left _ -> PathCtor core path ctors
        Right x -> x

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

instance PropLit PathCtor where
    (?=>) = impliesPathCtor
    (?/\) = undefined -- combinePathCtorAnd
    (?\/) = combinePathCtorOr
    litNot = Just . notPathCtor

-- SIMPLIFIERS

notReq Demonic = Demonic
notReq Angelic = Angelic
notReq (Req expr x) = Req expr (fromJust $ litNot x)

notPathCtor (PathCtor hill path ctrs) = newPathCtorAlways hill path ctrs2
    where ctrs2 = sort $ ctorNames (coreCtorData hill (head ctrs)) \\ ctrs


combineReqsAnd :: Req -> Req -> Reduce Req
combineReqsAnd (Req on1 (PathCtor hite path1 ctors1)) (Req on2 (PathCtor _ path2 ctors2))
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
reduceAnd (Req on1 (PathCtor hite path1 ctors1)) (Req on2 (PathCtor _ path2 ctors2))
    | on1 == on2 && path2 `subsetPath` path1 && ctors2 /= ctrs
    = Just $ Req on2 (PathCtor hite path2 ctrs)
    where ctrs = ctors1 `intersect` ctors2
reduceAnd _ x = Nothing


combinePathCtorOr :: PathCtor -> PathCtor -> Reduce PathCtor
combinePathCtorOr (PathCtor hite path1 ctors1) (PathCtor _ path2 ctors2)
        | path1 == path2 && finitePath path1
        = if length ctrs == length baseSet then Literal True else
          case newPathCtor hite path1 ctrs of
              Left x -> Literal x
              Right x -> Value x
    where
        ctrs = snub $ ctors2 ++ ctors1
        baseSet = ctorNames $ coreCtorData hite (head ctrs)
        
combinePathCtorOr _ _ = None


combineReqsOr :: Req -> Req -> Reduce Req
combineReqsOr (Req on1 pc1) (Req on2 pc2) =
    if on1 /= on2 then None else
    case pc1 ?\/ pc2 of
        Value x -> Value (Req on1 x)
        Literal x -> Literal x
        None -> None


-- impliesPair a b, a => b
impliesPair :: PathCtor -> PathCtor -> Bool
impliesPair (PathCtor hite path ctors) r2@(PathCtor _ path2 ctors2)
    | ctors `subset` ctors && newPathCtorAlways hite path ctors2 == r2 = True
    | ctors `subset` ctors2 && path2 `subsetPath` path = True
impliesPair _ _ = False


impliesPathCtor :: [(PathCtor, Bool)] -> PathCtor -> Maybe Bool
impliesPathCtor given req@(PathCtor hite path ctors) =
        if null ctors then Just False
        else if any doesImply given then Just True
        else if poss `subset` ctors then Just True
        else if ctors `disjoint` poss then Just False
        else Nothing
    where
        doesImply :: (PathCtor,Bool) -> Bool
        doesImply (r2,b) = b && impliesPair req r2

        -- calculate all possible constructors that might arise
        poss = foldr f baseSet given
        baseSet = ctorNames $ coreCtorData hite (headNote "Tram.Type.impliesReq" ctors)
        
        f (PathCtor _ path2 ctors2, False) poss
            | path2 == path && finitePath path
            = poss \\ ctors2
        
        f (PathCtor _ path2 ctors2, True) poss
            | path `subsetPath` path2
            = poss `intersect` ctors2

        f _ poss = poss
        

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
