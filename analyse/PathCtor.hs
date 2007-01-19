
module PathCtor where

import Path
import DataRep

import Yhc.Core
import General
import Data.Proposition
import Safe
import Data.Maybe
import Data.List


data PathCtor = PathCtor Core Path [CoreCtorName]

type BoolPathCtor = Either Bool PathCtor


instance Eq PathCtor where
    (PathCtor _ a1 b1) == (PathCtor _ a2 b2) = a1 == a2 && b1 == b2

instance Ord PathCtor where
    compare (PathCtor _ a1 b1) (PathCtor _ a2 b2) = compare (a1,b1) (a2,b2)

instance Show PathCtor where
    show (PathCtor _ path ctor) = show path ++ strSet ctor

-- SMART CONSTRUCTORS


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
        baseSet = ctorNames core (head ctors)

newPathCtorAlways :: Core -> Path -> [CoreCtorName] -> PathCtor
newPathCtorAlways core path ctors =
    case newPathCtor core path ctors of
        Left _ -> PathCtor core path ctors
        Right x -> x


-- UTILITIES

instance PropLit PathCtor where
    (?=>) = impliesPathCtor
    (?/\) = combinePathCtorAnd
    (?\/) = combinePathCtorOr
    litNot = Just . notPathCtor

-- SIMPLIFIERS

notPathCtor (PathCtor hill path ctrs) = newPathCtorAlways hill path ctrs2
    where ctrs2 = sort (ctorNames hill (head ctrs) \\ ctrs)



combinePathCtorAnd :: PathCtor -> PathCtor -> Reduce PathCtor
combinePathCtorAnd (PathCtor hite path1 ctors1) (PathCtor _ path2 ctors2)
        | path1 == path2
        = if null ctrs then Literal False else
          case newPathCtor hite path1 ctrs of
              Left x -> Literal x
              Right x -> Value x
    where
        ctrs = sort $ ctors2 `intersect` ctors1

combinePathCtorAnd pc1 pc2
        | isJust s1 || isJust s2
        = fromMaybe (combinePathCtorAnd t1 t2) (reduceAndWithImp t1 t2)
    where
        (s1,s2) = (reduceAnd pc1 pc2, reduceAnd pc1 pc2)
        (t1,t2) = (fromMaybe pc1 s1 , fromMaybe pc2 s2 )

combinePathCtorAnd _ _ = None



-- given that a predicate is anded, what must this one be
-- must make things smaller, or returns Nothing
reduceAnd :: PathCtor -> PathCtor -> Maybe PathCtor
reduceAnd (PathCtor hite path1 ctors1) (PathCtor _ path2 ctors2)
    | path2 `subsetPath` path1 && ctors2 /= ctrs
    = Just $ PathCtor hite path2 ctrs
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
        baseSet = ctorNames hite (head ctrs)
        
combinePathCtorOr _ _ = None


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
        baseSet = ctorNames hite (head ctors)
        
        f (PathCtor _ path2 ctors2, False) poss
            | path2 == path && finitePath path
            = poss \\ ctors2
        
        f (PathCtor _ path2 ctors2, True) poss
            | path `subsetPath` path2
            = poss `intersect` ctors2

        f _ poss = poss
        
