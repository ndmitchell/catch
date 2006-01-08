{- |
    General utility functions
    required by various different bits of the program
    and not particular regexp or fsa constrained
-}

module RegExp.General where

import List


-- | special hack so RegExp Char prints out well
-- since Char's are the most common thing by far
showNoQuote :: Show a => a -> String
showNoQuote x = case show x of
                   ['\'',y,'\''] -> [y]
                   ('\"':x@(_:_)) | last x == '\"' -> init x
                   y -> y


-- | find the transitive closure of a set
-- with a function to generate subsequent members from existing
-- put the elements in a sensible order, even though its meant to be a set
-- 
-- > transitive = transitiveBy (==)
transitive :: Eq a => (a -> [a]) -> [a] -> [a]
transitive = transitiveBy (==)

-- | like 'transitive' but with a user supplied equality function
transitiveBy :: (a -> a -> Bool) -> (a -> [a]) -> [a] -> [a]
transitiveBy eq next xs = f [] xs
    where
        f seen [] = []
        f seen (t:odo) | any (eq t) seen = f seen odo
                       | otherwise = t : f (t:seen) (next t ++ odo)



-- | ordering only applying to the second element of a pair
sndOrd :: Ord a => (b,a) -> (c,a) -> Ordering
a `sndOrd` b = snd a `compare` snd b

-- | equality only applying to the second element of a pair
sndEq :: Eq a => (b,a) -> (c,a) -> Bool
a `sndEq`  b = snd a == snd b



-- | Are two sets eq, given the appropriate equality test.
setEqBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
setEqBy f (x:xs) ys = case remElem f x ys of
                          Just a -> setEqBy f xs a
                          Nothing -> False
setEqBy f [] [] = True
setEqBy _ _ _ = False


-- | Remove an element from a set, using the given equality test.
--   Returns Nothing if the element was not in the set
remElem :: (a -> a -> Bool) -> a -> [a] -> Maybe [a]
remElem f a xs = g [] xs
    where
        g _ [] = Nothing
        g done (x:xs) | f x a = Just (done++xs)
                      | otherwise = g (x:done) xs


-- | Return a list of the members that occur exactly one
exactlyOnce :: Eq a => [a] -> [a]
exactlyOnce x = nx \\ (x \\ nx)
    where nx = nub x


-- | Split up into all prefix and suffix pairs
--
-- > divisions "abc" = [("","abc"),("a","bc"),("ab","c"),("abc","")] 
divisions :: [a] -> [([a], [a])]
divisions x = zip (inits x) (tails x)


-- | All possible selections
--
-- > selections "abc" = [("bc",'a'),("ac",'b'),("ab",'c')]
selections :: [a] -> [([a], a)]
selections xs = f [] xs
    where
        f done [] = []
        f done (x:xs) = ((done++xs), x) : f (done++[x]) xs


-- | map fst
fsts :: [(a,b)] -> [a]
fsts = map fst

-- | map snd
snds :: [(a,b)] -> [b]
snds = map snd

-- | The first argument must be smaller to return true.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = null $ xs \\ ys


-- | While the predicate is true, keep executing f on the value
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = if p x then while p f (f x) else x

-- | Like 'while' but output a list of the stages
whileList :: (a -> Bool) -> (a -> a) -> a -> [a]
whileList p f x = x : if p x then whileList p f (f x) else []

