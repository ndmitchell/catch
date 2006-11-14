
{- |
    For details see <http://www.cs.york.ac.uk/~ndm/projects/libraries.php>
    
    The idea behind this library is that the simplification of the predicate is
    handled automatically using the 'PredLit' instance.
-}

module Data.Predicate2(
    -- * Core Type Declarations
    Pred, Reduction(..), PredLit(..), PredLitNot(..),
    -- * Predicate Creation
    predTrue, predFalse, predLit, predAnd, predOr, predNot, predBool,
    -- * Extract and Test
    isFalse, isTrue,
    -- * Show
    showPred, showPredBy,
    -- * Play
    mapPredLit, allPredLit, mapPredLitM
    ) where


import Data.List
import Data.Maybe
import Control.Monad

import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap


-- * Debugging options

-- only to be used for developing
disableSimplify :: Bool
disableSimplify = False
    
    
-- * Core Type

-- | The abstract data type of a predicate.
--   Most users will want the type variable to be a member of 'PredLit'
data Pred a = PredTrue
			| PredFalse
			| Pred (Map.Map a Int) [IntSet.IntSet]
            deriving (Read, Show)


-- | How do two items combine to be reduced, for simplification rules.
data Reduction a = Same           -- ^ These two items do not simplify
                 | Single a       -- ^ Two items collapse to one item, same as 'Priority' infinite
                 | Value Bool     -- ^ Two items collapse to a boolean value
                 | Priority Int a -- ^ The items collapse, but with a given priority - higher gets done first

-- | A predicate that has simplifications on it, all methods are optional
class Ord a => PredLit a where
    -- | the first item implies the second
    (?=>) :: a -> a -> Bool
    -- | how two items combine under or
    (?\/) :: a -> a -> Reduction a
    -- | how two items combine under and
    (?/\) :: a -> a -> Reduction a
    -- | can a single value be collapse to a boolean
    simp :: a -> Maybe Bool
    
    simp x = Nothing
    x ?=> y = False
    x ?\/ y = Same
    x ?/\ y = Same


-- | A predicate that can also be negated, required for 'predNot' only
class PredLit a => PredLitNot a where
    -- | the negation of a literal
    litNot :: a -> Pred a



-- * Useful utilities

(??\/) :: PredLit a => a -> a -> Reduction a
a ??\/ b | disableSimplify = Same
		 | a == b = Single a
         | a ?=> b = Single b
         | otherwise = a ?\/ b


(??/\) :: PredLit a => a -> a -> Reduction a
a ??/\ b | disableSimplify = Same
	     | a == b = Single a
         | a ?=> b = Single a
         | otherwise = a ?/\ b



-- * Creators


-- | A constant True
predTrue :: Pred a
predTrue = PredTrue

-- | A constant False
predFalse :: Pred a
predFalse = PredFalse

-- | Create a predicate that is just a literal
predLit :: a -> Pred a
predLit x = Pred (Map.singleton x 0) [IntSet.singleton 0]




makeCompat :: PredLit a => [Pred a] -> (Map.Map a Int, [[IntSet.IntSet]])
makeCompat xs = (mp, map f xs)
	where
		mp = Map.fromAscList $ zip as [0..]
		as = mergeList [map fst $ Map.toAscList a | Pred a b <- xs]
		
		f (Pred a b) = map (IntSet.map g) b
			where
				mp2 = IntMap.fromList [(v, fromJust $ Map.lookup k mp) | (k,v) <- Map.toAscList a]
				g x = fromJust $ IntMap.lookup x mp2
		
		
mergeList xs = foldr f [] xs
	where
		f (x:xs) (y:ys) =
			case compare x y of
				EQ -> x : f xs ys
				LT -> x : f xs (y:ys)
				GT -> y : f (x:xs) ys
		
		f [] ys = ys
		f xs [] = xs



-- | Combine a list of predicates with and
predAnd :: PredLit a => [Pred a] -> Pred a
predAnd xs = undefined {- case items of
                [x] -> x
                xs | any isFalse xs -> predFalse
                   | otherwise -> PredAnd xs
    where
        items = nub $ filter (not . isTrue) $ solveTerms (??/\) $ concatMap fromAnd xs
-}

-- | Combine a list of predicates with or
predOr :: PredLit a => [Pred a] -> Pred a
predOr xs = undefined {- case items of
                [x] -> x
                xs | any isTrue xs -> predTrue
                   | otherwise -> PredOr xs
    where
        items = nub $ filter (not . isFalse) $ solveTerms (??\/) $ concatMap fromOr xs
-}

-- | Return True only if the predicate is definately False. Note that predFalse /is not/ not . predTrue
isFalse :: Pred a -> Bool
isFalse (PredFalse) = True
isFalse _ = False

-- | Return True only if the predicate is definately True
isTrue :: Pred a -> Bool
isTrue (PredTrue) = True
isTrue _ = False


-- | Create a predicate that matches the boolean value
predBool :: Bool -> Pred a
predBool True  = predTrue
predBool False = predFalse


demandBool :: Pred a -> Bool
demandBool x | isTrue x = True
             | isFalse x = False
             | otherwise = error "demandBool, error"

-- | Negate a predicate
predNot :: PredLitNot a => Pred a -> Pred a
predNot x = undefined {- =
    case x of
        PredOr  xs -> predAnd $ map predNot xs
        PredAnd xs -> predOr  $ map predNot xs
        PredLit x  -> litNot x-}


-- * Show

-- | Show a predicate nicely
showPred :: Show a => Pred a -> String
showPred x = showPredBy show x


-- | Show a predicate, with a special function for showing each element
showPredBy :: (a -> String) -> Pred a -> String
showPredBy f x = undefined {-
    case x of
        PredOr  [] -> "False"
        PredAnd [] -> "True"
        PredLit  a -> f a
        PredOr  xs -> disp 'v' xs
        PredAnd xs -> disp '^' xs
    where
        disp sym xs = "(" ++ mid ++ ")"
            where mid = concat $ intersperse [' ',sym,' '] $ map (showPredBy f) xs -}


-- * Eq

instance Eq a => Eq (Pred a) where
	a == b = undefined {-
    (PredLit a) == (PredLit b) = a == b
    (PredAnd a) == (PredAnd b) = sameSet a b
    (PredOr  a) == (PredOr  b) = sameSet a b
    _ == _ = False -}


-- * Maps and traversals

-- | Convert a predicate to a boolean
mapPredBool :: (a -> Bool) -> Pred a -> Bool
mapPredBool f = undefined -- demandBool . mapPredLit (predBool . f)

-- | Perform a map over every literal
mapPredLit :: PredLit b => (a -> Pred b) -> Pred a -> Pred b
mapPredLit f x = undefined {-
    case x of
        PredOr  xs -> predOr  $ fs xs
        PredAnd xs -> predAnd $ fs xs
        PredLit x  -> f x
    where
        fs = map (mapPredLit f) -}

-- | Perform a map over every literal, mondically
mapPredLitM :: (Monad m, PredLit b) => (a -> m (Pred b)) -> Pred a -> m (Pred b)
mapPredLitM f x = undefined {-
	case x of
		PredOr  xs -> liftM predOr  $ fs xs
		PredAnd xs -> liftM predAnd $ fs xs
		PredLit x  -> f x
	where
		fs = mapM (mapPredLitM f) -}


-- | Get all literals in a predicate
allPredLit :: Pred a -> [a]
allPredLit x = undefined {-
    case x of
        PredOr  xs -> fs xs
        PredAnd xs -> fs xs
        PredLit x  -> [x]
    where
        fs = concatMap allPredLit -}
