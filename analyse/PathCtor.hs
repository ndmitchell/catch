
module PathCtor(
    -- from this module
    BoolPathCtor(..), PathCtor(..),
    newPathCtor, newPathCtorAlways,
    equalPathCtor, falsePathCtor, truePathCtor,
    
    -- reexported from Path
    Path, emptyPath, ewpPath, blurPath,
    integrate, differentiate
    ) where

import Path
import DataRep

import Yhc.Core
import General
import Data.Proposition
import Safe
import Data.Maybe
import Data.List

import SmallCheck


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
        

-- SUPER STRONG EQUALITY

equalPathCtor :: PathCtor -> PathCtor -> Bool
equalPathCtor pc1@(PathCtor core p1 c1) pc2@(PathCtor _ p2 c2)
    | pc1 == pc2 = True
    | dat1 /= dat2 = False
    | ewp1 && ewp2 && c1 /= c2 = False
    | ewp1 /= ewp2 = False
    | otherwise = and [(differentiate p1 x, c1) `eq` (differentiate p2 x, c2)
                      | x <- validPaths]
    where
        eq (Nothing,_) (Nothing,_) = True
        eq (Just a, b) (Just c, d) = equalPathCtor (PathCtor core a b) (PathCtor core c d)
        eq _ _ = False
    
        (ewp1, ewp2) = (ewpPath p1, ewpPath p2)
        (dat1, dat2) = (getDat pc1, getDat pc2)
        
        -- the paths you can now follow
        validPaths = [x | ctr <- coreDataCtors dat1, (_, Just x) <- coreCtorFields ctr
                        , not ewp1 || coreCtorName ctr `elem` c1]
        
        -- getData, CoreData
        -- the type that you are currently in
        getDat (PathCtor _ (Path []) c) = coreCtorData core (head c)
        getDat (PathCtor _ (Path (PathAtom x : _)) _) = coreFieldData core x
        getDat (PathCtor _ (Path (PathStar x : _)) _) = coreFieldData core $ head x



-- OPERATIONS ON PATHS

-- is the first a subset of the second
subsetPath :: Path -> Path -> Bool
subsetPath (Path a) (Path b) = f a b
    where
        f (PathAtom x:xs) (PathAtom y:ys) | x == y = f xs ys
        f (PathStar x:xs) (PathStar y:ys) | x `subset` y = f xs ys
        f xs              (PathStar y:ys) = f xs ys
        f [] [] = True
        f _ _ = False


finitePath (Path x) = all (not . isPathStar) x


restrictPath :: Path -> [CoreFieldName] -> Path
restrictPath (Path x) allow = Path (concatMap f x)
    where
        f (PathStar x) = if null x2 then [] else [PathStar x2]
            where x2 = filter (`elem` allow) x
        f x = [x]


-- TESTING INSTANCES


falsePathCtor :: PathCtor
falsePathCtor = PathCtor testCore emptyPath []

truePathCtor :: PathCtor
truePathCtor = PathCtor testCore emptyPath ["A","B","C","D"]


testCore :: Core
testCore = Core [] [] [testData] []
    where
        testData =
            CoreData "ABCD" []
                [CoreCtor "A" [("ABCD",Just "as"), ("*",Just "a")]
                ,CoreCtor "B" [("ABCD",Just "bs")]
                ,CoreCtor "C" [("*",Just "c")]
                ,CoreCtor "D" []
                ]


newtype CtorSet = CtorSet {ctorSet :: [CoreCtorName]}
                  deriving Show


-- SmallCheck

instance Serial CtorSet where
    -- return all sets with 0..n ctors in, above 4 it just returns everything
    -- currently returns all - they are two important to throw away the big ones
    series n = [CtorSet w | w <- whole] --, length w <= n]
        where whole = map (map (:[]) . filter (/= ' ')) $ sequence [" A"," B"," C"," D"]

instance Serial Path where
    -- return all paths of n or below, so you get all the combinations properly
    series n = concatMap f [0..n]
        where
            f n = map (blurPath testCore . Path . map PathAtom) path
                where path = sequence (replicate n ["as","a","bs","c"])

instance Serial PathCtor where
    series = cons2 pathCtor
        where pathCtor a (CtorSet b) = PathCtor testCore a b
