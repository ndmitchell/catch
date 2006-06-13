
{- |
    For details see <http://www.cs.york.ac.uk/~ndm/projects/libraries.php>
    
    The idea behind this library is that the simplification of the predicate is
    handled automatically using the 'PredLit' instance.
-}

module Data.Predicate(
    -- * Core Type Declarations
    Pred, Reduction(..), PredLit(..), PredLitNot(..),
    -- * Predicate Creation
    predTrue, predFalse, predLit, predAnd, predOr, predNot, predBool,
    -- * Extract and Test
    fromAnd, fromOr, fromLit,
    isFalse, isTrue, isLit,
    -- * Show
    showPred, showPredBy,
    -- * Play
    mapPredLit, allPredLit, foldPred,
    -- * Null instance
    PredNull, demandBool, mapPredBool
    ) where


import Data.List
import Data.Maybe


-- * Debugging options

-- only to be used for developing
disableSimplify :: Bool
disableSimplify = False
    
    
-- * Core Type

-- | The abstract data type of a predicate.
--   Most users will want the type variable to be a member of 'PredLit'
data Pred a = PredOr  [Pred a]
            | PredAnd [Pred a]
            | PredLit a
            deriving (Read, Show)


-- | How do two items combine to be reduced, for simplification rules.
data Reduction a = Same           -- ^ These two items do not simplify
                 | Single a       -- ^ Two items collapse to one item, same as 'Priority' infinite
                 | Value Bool     -- ^ Two items collapse to a boolean value
                 | Priority Int a -- ^ The items collapse, but with a given priority - higher gets done first

-- | A predicate that has simplifications on it, all methods are optional
class PredLit a where
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

-- | A Null PredLit type, if required
instance PredLit () where

data PredNull = PredNull
instance PredLit PredNull where

-- * Useful utilities

(??\/) :: PredLit a => a -> a -> Reduction a
a ??\/ b | disableSimplify = Same
         | a ?=> b = Single b
         | otherwise = a ?\/ b


(??/\) :: PredLit a => a -> a -> Reduction a
a ??/\ b | disableSimplify = Same
         | a ?=> b = Single a
         | otherwise = a ?/\ b


reduceList :: PredLit a => (a -> a -> Reduction a) -> [a] -> [Pred a]
reduceList pair xs = f xs
    where
        f xs = g Nothing xs (allPairs xs)
        
        
        g Nothing orig [] = map simpItem orig
        g (Just (_,res)) orig [] = res
        g pending orig (((a,b),rest):remainder) =
            case pair a b of
                Same -> g pending orig remainder
                Single x -> f (x:rest)
                Value b -> predBool b : f rest
                Priority pri x -> case pending of
                    Just (p2,res) | p2 >= pri -> g pending orig remainder
                    _ -> g (Just (pri, f (x:rest))) orig remainder

{-            
    
    
        f [] = []
        f (x:xs) = comps ++ g maxBound Nothing [] x (map fromLit lits)
            where (lits, comps) = partition isLit (f xs)
        
        g mx (Just pri) acc x [] = g pri Nothing [] x acc
        g mx Nothing    acc x [] = simpList (x:acc)
        g mx pri acc x (y:ys) = case pair x y of
                             Same -> g (y:acc) x ys
                             Single a -> g [] a (acc++ys)
                             Value b -> predBool b : g acc x ys
-}


-- find all pairs in a list
allPairs :: [a] -> [((a,a),[a])]
allPairs [] = []
allPairs (x:xs) = [((x,y),ys) | (y,ys) <- allElems xs] ++
                  [(y,x:ys)   | (y,ys) <- allPairs xs]


allElems :: [a] -> [(a, [a])]
allElems [] = []
allElems (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- allElems xs]


simpItem :: PredLit a => a -> Pred a
simpItem x = case simp x of
                  Just b | not disableSimplify -> predBool b
                  _ -> predLit x


-- * Simple tests and extractors

-- | return a list of the items, if the item is not a 'predAnd' its the singleton list
fromAnd :: Pred a -> [Pred a]
fromAnd (PredAnd x) = if null x then [predTrue] else x
fromAnd x           = [x]


-- | return a list of the items, if the item is not a 'predOr' its the singleton list
fromOr :: Pred a -> [Pred a]
fromOr (PredOr x) = if null x then [predFalse] else x
fromOr x          = [x]



-- * Creators


-- | A constant True
predTrue :: Pred a
predTrue = PredAnd []

-- | A constant False
predFalse :: Pred a
predFalse = PredOr []

-- | Create a predicate that is just a literal
predLit :: a -> Pred a
predLit x = PredLit x

-- | Crashes if 'isLit' is False
fromLit :: Pred a -> a
fromLit (PredLit x) = x


solveTerms f items = lits2 ++ terms
    where
        lits2 = reduceList f (map fromLit lits)
        (lits, terms) = partition isLit items


-- | Combine a list of predicates with and
predAnd :: PredLit a => [Pred a] -> Pred a
predAnd xs = case items of
                [x] -> x
                xs | any isFalse xs -> predFalse
                   | otherwise -> PredAnd xs
    where
        items = filter (not . isTrue) $ solveTerms (??/\) $ concatMap fromAnd xs


-- | Combine a list of predicates with or
predOr :: PredLit a => [Pred a] -> Pred a
predOr xs = case items of
                [x] -> x
                xs | any isTrue xs -> predTrue
                   | otherwise -> PredOr xs
    where
        items = filter (not . isFalse) $ solveTerms (??\/) $ concatMap fromOr xs


-- | Return True only if the predicate is definately False. Note that predFalse /is not/ not . predTrue
isFalse :: Pred a -> Bool
isFalse (PredOr  []) = True
isFalse (PredAnd xs) = any isFalse xs
isFalse (PredOr  xs) = all isFalse xs
isFalse _ = False

-- | Return True only if the predicate is definately True
isTrue :: Pred a -> Bool
isTrue (PredAnd []) = True
isTrue (PredAnd xs) = all isTrue xs
isTrue (PredOr  xs) = any isTrue xs
isTrue _ = False


-- | Is a predicate just a literal
isLit :: Pred a -> Bool
isLit (PredLit x) = True
isLit _ = False


-- | Create a predicate that matches the boolean value
predBool :: Bool -> Pred a
predBool True  = predTrue
predBool False = predFalse


demandBool :: Pred PredNull -> Bool
demandBool x | isTrue x = True
             | isFalse x = False
             | otherwise = error "demandBool, error"

-- | Negate a predicate
predNot :: PredLitNot a => Pred a -> Pred a
predNot x =
    case x of
        PredOr  xs -> predAnd $ map predNot xs
        PredAnd xs -> predOr  $ map predNot xs
        PredLit x  -> litNot x


-- * Show

-- | Show a predicate nicely
showPred :: Show a => Pred a -> String
showPred x = showPredBy show x


-- | Show a predicate, with a special function for showing each element
showPredBy :: (a -> String) -> Pred a -> String
showPredBy f x =
    case x of
        PredOr  [] -> "False"
        PredAnd [] -> "True"
        PredLit  a -> f a
        PredOr  xs -> disp 'v' xs
        PredAnd xs -> disp '^' xs
    where
        disp sym xs = "(" ++ mid ++ ")"
            where mid = concat $ intersperse [' ',sym,' '] $ map (showPredBy f) xs


-- * Eq

instance Eq a => Eq (Pred a) where
    (PredLit a) == (PredLit b) = a == b
    (PredAnd a) == (PredAnd b) = sameSet a b
    (PredOr  a) == (PredOr  b) = sameSet a b
    _ == _ = False


sameSet a b | length a /= length b = False
            | otherwise = f [] a b
    where
        f [] [] [] = True
        f acc (a:as) (b:bs) | a == b = f [] as (acc ++ bs)
                            | otherwise = f (b:acc) (a:as) bs
        f _ _ _ = False                        


-- * Maps and traversals

-- | Convert a predicate to a boolean
mapPredBool :: (a -> Bool) -> Pred a -> Bool
mapPredBool f = demandBool . mapPredLit (predBool . f)

-- | Perform a map over every literal
mapPredLit :: PredLit b => (a -> Pred b) -> Pred a -> Pred b
mapPredLit f x =
    case x of
        PredOr  xs -> predOr  $ fs xs
        PredAnd xs -> predAnd $ fs xs
        PredLit x  -> f x
    where
        fs = map (mapPredLit f)

-- | Get all literals in a predicate
allPredLit :: Pred a -> [a]
allPredLit x =
    case x of
        PredOr  xs -> fs xs
        PredAnd xs -> fs xs
        PredLit x  -> [x]
    where
        fs = concatMap allPredLit


-- | or then and
foldPred :: ([a] -> a) -> ([a] -> a) -> (b -> a) -> Pred b -> a
foldPred for fand fone x =
    case x of
        PredOr  x -> for  $ f x
        PredAnd x -> fand $ f x
        PredLit x -> fone x
    where
        f = map (foldPred for fand fone)
