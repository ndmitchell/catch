{- |
    Various properties on regular expressions.
-}
module RegExp.Prop where

import RegExp.Type

---------------------------------------------------------------------
-- * Properties

-- | ^ is a member of L(x)
isEwp :: (Eq a, Show a) => RegExp a -> Bool
isEwp (RegConcat x) = all isEwp x
isEwp (RegUnion  x) = any isEwp x
isEwp (RegKleene x) = True
isEwp (RegOmega   ) = False
isEwp (RegLit    x) = False


-- | L(x) = {}
isEmpty :: (Eq a, Show a) => RegExp a -> Bool
isEmpty (RegConcat x) = any isEmpty x
isEmpty (RegUnion  x) = all isEmpty x
isEmpty (RegOmega   ) = True
isEmpty _             = False


-- | L(x) = {a, ...} where a is a string of at least length 1
isNonEmpty :: (Eq a, Show a) => RegExp a -> Bool
isNonEmpty (RegKleene x) = isNonEmpty x
isNonEmpty (RegConcat x) = not (isEmpty (RegConcat x)) && any isNonEmpty x
isNonEmpty (RegUnion  x) = any isNonEmpty x
isNonEmpty (RegOmega   ) = False
isNonEmpty (RegLit    _) = True


-- | Is the string a  member of L(x)
isMember :: (Eq a, Show a) => [a] -> RegExp a -> Bool
isMember [    ] x = isEwp x
isMember (y:ys) x = not (isEmpty x) && isMember ys (quotient y x)


-- | Is #(L(x)) finite.
-- It is infinite if there is a NonEmpty inside a star.
isFinite :: (Eq a, Show a) => RegExp a -> Bool
isFinite (RegKleene x) = not (isNonEmpty x)
isFinite (RegConcat x) = not (isEmpty (RegConcat x)) && any isFinite x
isFinite (RegUnion  x) = any isFinite x
isFinite (RegOmega   ) = True
isFinite (RegLit    _) = True


-- | What is the star depth of the regular expression.
--   Maximum nested star's
starDepth  :: (Eq a, Show a) => RegExp a -> Int
starDepth (RegKleene x) = starDepth x + 1
starDepth (RegConcat x) = maximum (map starDepth x)
starDepth (RegUnion  x) = maximum (map starDepth x)
starDepth (RegOmega   ) = 0
starDepth (RegLit    _) = 0


-- | Is 0 present anywhere within the expression.
hasOmega :: (Eq a, Show a) => RegExp a -> Bool
hasOmega (RegKleene x) = hasOmega x
hasOmega (RegConcat x) = any hasOmega x
hasOmega (RegUnion  x) = any hasOmega x
hasOmega (RegOmega   ) = True
hasOmega (RegLit    _) = False



---------------------------------------------------------------------
-- * Literal Operations

-- | All the literals in the expression, as many times as they appear, in order.
--   Use 'nub' to get the list without duplicates.
allLits :: (Eq a, Show a) => RegExp a -> [a]
allLits (RegKleene x) = allLits x
allLits (RegConcat x) = concatMap allLits x
allLits (RegUnion  x) = concatMap allLits x
allLits (RegOmega   ) = []
allLits (RegLit    x) = [x]


-- | Modify all literals in the expression
mapLits :: (Eq a, Show a) => (a -> a) -> RegExp a -> RegExp a
mapLits f (RegKleene x) = RegKleene $ mapLits f x
mapLits f (RegConcat x) = RegConcat $ map (mapLits f) x
mapLits f (RegUnion  x) = RegUnion  $ map (mapLits f) x
mapLits f (RegOmega   ) = RegOmega
mapLits f (RegLit    x) = RegLit $ f x


---------------------------------------------------------------------
-- * Quotient

-- | Take the quotient of a regular expression with respect to a character.
--   For more info see <http://en.wikipedia.org/wiki/Left_quotient>
quotient :: (Eq a, Show a) => a -> RegExp a -> RegExp a
quotient y (RegLit x) = if x == y then regLambda else regOmega

quotient y (RegConcat [x]   ) = quotient y x
quotient y (RegConcat (x:xs))
            | isEwp x = regUnion [regConcat (quotient y x:xs), quotient y (regConcat xs)]
            | otherwise = regConcat (quotient y x:xs)
                          
quotient y (RegUnion xs) = regUnion (map (quotient y) xs)
quotient y (RegKleene x) = regConcat [quotient y x, regKleene x]

quotient y (RegOmega   ) = regOmega


-- what are the next possible items
nextChar :: (Eq a, Show a) => RegExp a -> [a]
nextChar (RegLit x) = [x]

nextChar (RegConcat [x]) = nextChar x
nextChar (RegConcat (x:xs))
            | isEwp x = nextChar x ++ nextChar (RegConcat xs)
            | otherwise = nextChar x

nextChar (RegUnion xs) = concatMap nextChar xs
nextChar (RegKleene x) = nextChar x

nextChar (RegOmega   ) = []
