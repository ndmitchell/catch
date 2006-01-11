{- |
    This module defines the fundamental data type for regular expressions
    basic operations on regular expressions are also defined here

    For comment purposes, 0 = omega, ^ = lambda or 0*
-}

module RegExp.Type(
    -- * The data constructors
    RegExp(..), RegExpChar,
    textLambda, textOmega,
    
    -- ** Basic pattern match tests
    isOmega, isLambda, isRegLit,
    fromConcat, fromRegLit,
    
    -- * Creation Functions
    -- A set of creation functions, that do basic minimization
    regOmega, regLambda, regLit, regUnion, regConcat, regKleene,
    -- and recreate a regular expression with these simplifications
    reduceRegExp,
    
    mapRegExp,

    -- * Smaller
    smaller
    ) where


import List
import RegExp.General

---------------------------------------------------------------------
-- DATA STRUCTURE

-- | The main regular expression data structure
data (Eq a, Show a) => RegExp a
              = RegKleene (RegExp a)
              | RegConcat [RegExp a]
              | RegUnion  [RegExp a]
              | RegOmega
              | RegLit a

-- | A special type to represent char's, given this is most common
type RegExpChar = RegExp Char

-- | Is the regular expression exactly 'RegOmega', for equal to use 'isEmpty'
isOmega :: (Eq a, Show a) => RegExp a -> Bool
isOmega RegOmega = True; isOmega _ = False

-- | Is the regular expression exactly lambda, for equal to use 'isEwp' and 'isNonEmpty'
isLambda :: (Eq a, Show a) => RegExp a -> Bool
isLambda (RegKleene RegOmega) = True; isLambda _ = False


-- | Symbol to use for a lambda, ^
textLambda :: String
textLambda = "1"

-- | Symbol to use for an omega, 0
textOmega :: String
textOmega = "0"

-- | And a play class
mapRegExp :: (Eq a, Show a) => (RegExp a -> RegExp a) -> RegExp a -> RegExp a
mapRegExp f x = f $ case x of
        RegKleene y -> regKleene (f y)
        RegConcat ys -> regConcat (fs ys)
        RegUnion  ys -> regUnion  (fs ys)
        x -> x
    where
        fs = map (mapRegExp f)


fromConcat :: (Eq a, Show a) => RegExp a -> [RegExp a]
fromConcat (RegKleene RegOmega) = []
fromConcat (RegConcat xs) = xs
fromConcat x = [x]

fromRegLit :: (Eq a, Show a) => RegExp a -> a
fromRegLit (RegLit x) = x

isRegLit :: (Eq a, Show a) => RegExp a -> Bool
isRegLit (RegLit _) = True
isRegLit _ = False

---------------------------------------------------------------------
-- COMPOSITION AND REDUCING FORMULA

-- | Performs no simplification
regLit :: (Eq a, Show a) => a -> RegExp a
regLit  x = RegLit x

-- | Performs no simplification
regOmega :: (Eq a, Show a) => RegExp a
regOmega  = RegOmega

-- | Performs no simplification
regLambda :: (Eq a, Show a) => RegExp a
regLambda = RegKleene RegOmega

-- | a** -> a*, (a*+b)* -> (a+b)*
regKleene :: (Eq a, Show a) => RegExp a -> RegExp a
regKleene (RegUnion  x) = RegKleene $ regUnion $ map f x
    where
        f (RegKleene x) = x
        f x = x
        
regKleene (RegKleene x) = RegKleene x
regKleene x             = RegKleene x

-- | a.0 -> 0, a.^ -> a
regConcat :: (Eq a, Show a) => [RegExp a] -> RegExp a
regConcat xs = if any isOmega res then regOmega else regCU RegConcat regLambda res
    where
        res = concatMap f xs
        
        f (RegConcat        x) = x
        f (RegKleene RegOmega) = []
        f x                    = [x]

-- | a+a -> a, a+0 -> a
regUnion :: (Eq a, Show a) => [RegExp a] -> RegExp a
regUnion xs = regCU RegUnion regOmega (nub (concatMap f xs))
    where
        f (RegUnion x) = x
        f (RegOmega  ) = []
        f x            = [x]

regCU f g []  = g
regCU f g [x] = x
regCU f g xs  = f xs



-- | Rebuilds a regular expression using the simplification creation functions.
--   Incorporates all the simplifications of the basic constructors, but no more.
reduceRegExp :: (Eq a, Show a) => RegExp a -> RegExp a
reduceRegExp x = mapRegExp f x
    where
        f (RegKleene x) = regKleene x
        f (RegConcat x) = regConcat x
        f (RegUnion  x) = regUnion  x
        f (RegOmega   ) = regOmega
        f (RegLit    x) = regLit x


---------------------------------------------------------------------
-- SIMILARITY

-- | Operator shorthand for 'similar'
instance (Eq a, Show a) => Eq (RegExp a) where
    (RegKleene a) == (RegKleene b) = a == b
    (RegConcat a) == (RegConcat b) = a == b
    (RegUnion  a) == (RegUnion  b) = length a == length b && setEqBy (==) a b
    (RegOmega   ) == (RegOmega   ) = True
    (RegLit    a) == (RegLit    b) = a == b
    _ == _ = False
    


---------------------------------------------------------------------
-- SMALLER

-- | Given a regular expression, give back a list of RegExps which are /smaller/ in some way.
--   If there are many items in a level, return all but one.
--   If there is only one item, then promote it.
--   Every returned regular expression must be smaller, but unlikely to be equivalent.
smaller :: (Eq a, Show a) => RegExp a -> [RegExp a]
smaller (RegKleene x) = x : [RegKleene y | y <- smaller x]
smaller (RegLit    x) = []
smaller (RegOmega   ) = []
smaller (RegConcat x) = smallerList RegConcat x
smaller (RegUnion  x) = smallerList RegUnion  x


-- | Given a list and a ctor, figure out the smaller expressions
smallerList :: (Eq a, Show a) => ([RegExp a] -> RegExp a) -> [RegExp a] -> [RegExp a]
smallerList ctor [x] = x : [ctor [y] | y <- smaller x]
smallerList ctor xs  = concatMap f (init $ divisions xs)
    where f (done, t:odo) = ctor (done ++ odo) : [ctor (done ++ [y] ++ odo) | y <- smaller t]


