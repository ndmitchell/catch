
module RegExp.StarFactor() where

import RegExp.Type
import RegExp.Parse
import General.Similar
import RegExp.General
import RegExp.Factorise

import List
import Maybe


sample = regUnion [regConcat [regKleene (regLit 'x'), regLit 'x', regLit 'x'], regLit 'x']


data StarFactor a = StarConcat [StarFactor a]
                  | StarUnion  [StarFactor a]
                  | StarKleene (StarFactor a) Factor
                  | StarLit    a
                  | StarOmega
                  deriving Show


data Factor = Ran Int Int
            | Uni [Factor]
            | Con [Factor]
            deriving (Eq, Show)

instance Eq a => Similar (StarFactor a) where
    (StarKleene a s1) ~= (StarKleene b s2) = a ~= b && s1 == s2
    (StarConcat a) ~= (StarConcat b) = length a == length b && and (zipWith (~=) a b)
    (StarUnion  a) ~= (StarUnion  b) = length a == length b && setEqBy (~=) a b
    (StarOmega   ) ~= (StarOmega   ) = True
    (StarLit    a) ~= (StarLit    b) = a == b
    _ ~= _ = False
            

starToRegExp :: Eq a => StarFactor a -> RegExp a
starToRegExp x = case x of
        StarConcat xs -> regConcat (map starToRegExp xs)
        StarUnion  xs -> regUnion  (map starToRegExp xs)
        StarLit    x  -> regLit x
        StarOmega     -> regOmega
        StarKleene x a -> f (starToRegExp x) a
    where
        f x (Ran from to) | from == inf && to == inf = regKleene x
                          | from == to = regConcat (replicate from x)
                          | to == inf = regConcat (regKleene x : replicate from x)
                          | otherwise =
            regConcat (replicate (to-from) (regUnion [regLambda,x]) ++ replicate from x)
        
        f x (Uni as) = regUnion  $ map (f x) as
        f x (Con as) = regConcat $ map (f x) as
    
    



regExpToStar :: (Show a, Eq a) => RegExp a -> StarFactor a
regExpToStar x = case x of
    RegConcat xs -> starConcat (map regExpToStar xs)
    RegUnion  xs -> starUnion  (map regExpToStar xs)
    RegKleene x  -> starKleene (regExpToStar x)
    RegLit    a  -> StarLit a
    RegOmega     -> StarOmega


starConcat :: Eq a => [StarFactor a] -> StarFactor a
starConcat xs = case f (concatMap explode xs) of
                    [x] -> x
                    xs -> StarConcat xs
    where
        f (StarKleene x1 s1 : StarKleene x2 s2 : xs) | x1 ~= x2 = f (StarKleene x1 (con [s1,s2]) : xs)
        f (StarKleene x1 s1 : x2 : xs) | x1 ~= x2 = f (StarKleene x1 (con [s1, Ran 1 1]) : xs)
        f (x2 : StarKleene x1 s1 : xs) | x1 ~= x2 = f (StarKleene x1 (con [s1, Ran 1 1]) : xs)
        f (x: xs) = x : f xs
        f [] = []
        
        explode (StarConcat x) = x
        explode x = [x]
        

starUnion :: Eq a => [StarFactor a] -> StarFactor a
starUnion xs = case applyFunc f (g (concatMap explode xs)) of
                [x] -> x
                xs -> StarUnion xs
    
    where
        explode (StarUnion x) = x
        explode x = [x]
        
        f (StarKleene StarOmega _) (StarKleene x y) | hasZero y = Just $ StarKleene x y
        f _ _ = Nothing
        
        g xs = if any isStarLambda xs then map h xs else xs
        
        isStarLambda (StarKleene _ x) = hasZero x
        isStarLambda _ = False
        
        h (StarKleene a b) = StarKleene a (uni [Ran 0 0, b])
        h x = StarKleene x (Ran 0 1)


starKleene :: StarFactor a -> StarFactor a
starKleene x = StarKleene x (Ran 0 inf)

inf :: Int
inf = -1

con :: [Factor] -> Factor
con xs = g $ sumRan rans : other
    where
        f (Con x) = x
        f x = [x]
        
        g [x] = x
        g xs = Con xs
        
        (rans, other) = partition isRan (concatMap f xs)


sumRan [x] = x
sumRan (Ran a1 b1 : Ran a2 b2 : xs) = sumRan (Ran (f a1 a2) (f b1 b2) : xs)
    where
        f n m | n == inf || m == inf = inf
              | otherwise = n+m




uni :: [Factor] -> Factor
uni xs = case reduceListAssoc f xs of
            [x] -> x
            xs -> Uni xs
    where
        f (Ran a1 b1) (Ran a2 b2) | a1 <<= a2 && b1 >>= b2 = Just (Ran a1 b1)
        f _ _ = Nothing
        
        x >>= y = g x >= g y
        x <<= y = g x <= g y
        
        g x = if inf == x then maxBound else x
        


{-
oneStar :: [Factor] -> [Factor]
oneStar x = if any isStar x then Star : filter (not . isStar) x else x
    where isStar = (==) Star


joinLit :: (Int -> Int -> Int) -> [Factor] -> [Factor]
joinLit f xs = if null lits then reals else Lit (foldr1 f (map fromLit lits)) : reals
    where
        (lits, reals) = partition isLit xs
        
        isLit (Lit _) = True; isLit _ = False
        fromLit (Lit x) = x
-}

{-
isStar Star = True
isStar (Con xs) = all isStar xs
isStar (Uni xs) = any isStar xs
isStar _ = False
-}

isRan (Ran _ _) = True; isRan _ = False

hasZero (Ran n m) = n == 0
hasZero (Con x) = all hasZero x
hasZero (Uni x) = any hasZero x


-- stolen from pred :)
-- pita because of Eq
applyFunc :: Eq a => (StarFactor a -> StarFactor a -> Maybe (StarFactor a)) -> [StarFactor a] -> [StarFactor a]
applyFunc f [] = []
applyFunc f (x:xs) =
        if all isNothing res then
            x : applyFunc f xs
        else
            g (zip res xs)
    where
        res = map (f x) xs
        
        g ((Nothing,y):ys) = y : g ys
        g ((Just a ,y):ys) = a : map snd ys
        g _ = error "applyFunc failed, logic error"



-- MOVE TO GENERAL


reduceList :: (a -> a -> Maybe a) -> [a] -> [a]
reduceList f [] = []
reduceList f (x:xs) =
        if all isNothing res then
            x : reduceList f xs
        else
            g (zip res xs)
    where
        res = map (f x) xs
        
        g ((Nothing,y):ys) = y : g ys
        g ((Just a ,y):ys) = a : map snd ys
        g _ = error "applyFunc failed, logic error"


reduceListAssoc :: (a -> a -> Maybe a) -> [a] -> [a]
reduceListAssoc f xs = reduceList g xs
    where
        g a b = case f a b of
                    Just a -> Just a
                    Nothing -> f b a
