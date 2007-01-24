
module PathCtor(
    -- from this module
    BoolPathCtor(..), PathCtor(..),
    newPathCtor, newPathCtorAlways,
    falsePathCtor, truePathCtor,
    
    -- reexported from Path
    Path, emptyPath, ewpPath, blurPath,
    integrate, differentiate
    ) where

import Path
import DataRep

import Yhc.Core
import General
import Control.Monad
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
    
    
-- Useful Core utilities
getAllCtors :: Core -> [CoreCtorName] -> [CoreCtorName]
getAllCtors core = snub . ctorNames core . head   


getAllFields :: Core -> [CoreCtorName] -> [CoreFieldName]
getAllFields core = snub . concatMap (map (fromJust . snd) . coreCtorFields . coreCtor core)



-- SMART CONSTRUCTORS

---------------------------------------------------------------------
-- SINGLE RULE SIMPLIFICATION

-- RULES
-- given a constructor set C
-- CS is the complete set, valid because of C's type
-- fs is the set of fields coming from C
-- 0 is the empty set

-- = Path Restriction =
-- x.y*{C} => x.(y union fs)*{C}
-- x.0*{C} => x{C}

-- = Completion =
-- x.y{C} | C == CS => True

-- = Nothing =
-- {0} => False
-- x.y*{0} => x{0}
-- x.y{0} => x{CS \\ y}


newPathCtor :: Core -> Path -> [CoreCtorName] -> BoolPathCtor
newPathCtor core (Path path) ctors =
        if not (null ctors) && allCtors == sctors then Left True else f (concatMap goodPath path) sctors
    where
        -- no empty stars, all must be sorted
        goodPath (PathStar []) = []
        goodPath (PathStar x) = [PathStar (snub x)]
        goodPath x = [x]
        
        allCtors = getAllCtors core ctors
        sctors = snub ctors
        

        f path ctors
            -- Nothing
            | null ctors = if null path then Left False
                           else if isPathStar lastpath then f initpath ctors
                           else let badctor = coreCtorName $ coreFieldCtor core $ fromPathAtom lastpath
                                in f initpath (badctor `delete` ctorNames core badctor)

            -- Path Restriction
            | not (null path) && isPathStar lastpath && ys /= ys2 =
                    let path2 = initpath ++ [PathStar ys2 | not $ null ys2]
                    in newPathCtor core (Path path2) ctors

            -- Fallthrough    
            | otherwise = Right $ PathCtor core (Path path) ctors
          where
            fields = getAllFields core ctors
            ys = fromPathStar lastpath
            ys2 = filter (`elem` fields) ys

            (initpath, lastpath) = (init path, last path)


newPathCtorAlways :: Core -> Path -> [CoreCtorName] -> PathCtor
newPathCtorAlways core path ctors =
    case newPathCtor core path ctors of
        Left _ -> PathCtor core path ctors
        Right x -> x

---------------------------------------------------------------------

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


---------------------------------------------------------------------
-- OR SIMPLIFICATION

-- RULES (not yet implemented)
-- a.b{C} v d.e{F} | a == d (not star) => a(b{C} v e{F})
-- {A} v {B} => {A `union` B}
-- {A} v b.c{D} | b `elem` A => True
--              | otherwise  => b.c{D}
-- {A} v x*{C} | C `superset` A ^ x `notin` A => x*{C}


combinePathCtorOr :: PathCtor -> PathCtor -> Reduce PathCtor
combinePathCtorOr (PathCtor core (Path path1) ctors1) (PathCtor _ (Path path2) ctors2) =
        f (path1, ctors1) (path2, ctors2)
    where
    
        f ([], a) ([], b) = if new == full then Literal True else Value (PathCtor core (Path []) new)
            where
                new = snub $ a ++ b
                full = getAllCtors core a

        f (PathAtom a:as, a1) (PathAtom b:bs, b1) | a == b =
            case f (as,a1) (bs,b1) of
                Literal x -> Literal x
                Value (PathCtor core (Path p) c) -> Value $ PathCtor core (Path (PathAtom a:p)) c
                None -> None
        
        f (x:xs, a) ([], b) = f ([], b) (x:xs, a)
        
        f ([], a) (PathAtom x:xs, b) | x `elem` getAllFields core a = Literal True
                                     | otherwise = Value $ PathCtor core (Path (PathAtom x:xs)) b

        f ([], a) ([PathStar x] , b) | x `disjoint` getAllFields core a && all (`elem` b) a
                                     = Value $ PathCtor core (Path [PathStar x]) b

        f _ _ = None
        
---------------------------------------------------------------------


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
    -- currently returns all - they are two important to throw away the big ones
    series n = [CtorSet w | w <- whole] --, length w <= n]
        where whole = map (map (:[]) . filter (/= ' ')) $ sequence [" A"," B"," C"," D"]

instance Serial Path where
    -- return all paths of n or below, so you get all the combinations properly
    series n = map (blurPath testCore . Path . map PathAtom) path
        where path = sequence (replicate n ["as","a","bs","c"])

instance Serial PathCtor where
    series = cons2 pathCtor
        where pathCtor a (CtorSet b) = PathCtor testCore a b
