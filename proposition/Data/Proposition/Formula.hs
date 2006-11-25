
module Data.Proposition.Formula(Formula) where

import Data.Proposition.Internal
import Data.List
import Data.Maybe
import Control.Monad


data Formula a = Lit a
               | Not a
               | Or  [Formula a]
               | And [Formula a]



instance Ord a => Ord (Formula a) where
    compare = compareFormula


sortFormula :: Ord a => Formula a -> Formula a
sortFormula (Or  x) = Or  $ sortFormulas x
sortFormula (And x) = And $ sortFormulas x
sortFormula x = x


sortFormulas :: Ord a => [Formula a] -> [Formula a]
sortFormulas xs = sortBy compareFormula xs


compareFormula :: Ord a => Formula a -> Formula a -> Ordering
compareFormula (Lit a) (Lit b) = compare a b
compareFormula (Or  x) (Or  y) = compareList x y
compareFormula (And x) (And y) = compareList x y
compareFormula x y = asInt x `compare` asInt y
    where
        asInt (Lit _) = 0
        asInt (Or  _) = 1
        asInt (And _) = 2


compareList :: Ord a => [Formula a] -> [Formula a] -> Ordering
compareList [] [] = EQ
compareList [] _  = LT
compareList _  [] = GT
compareList (x:xs) (y:ys) | x == y = compareList xs ys
                          | otherwise = compare x y






instance Prop Formula where
    propTrue = predTrue
    propFalse = predFalse
    
    propIsTrue = isTrue
    propIsFalse = isFalse
    
    propLit = Lit
    propAnd a b = predAnd [a,b]
    propOr  a b = predOr  [a,b]
    propNot = predNot

    propMapM = mapPropLitM
    
    propFold = foldFormula


-- * Debugging options

-- only to be used for developing
disableSimplify :: Bool
disableSimplify = False
    
    
-- * Core Type

-- | The abstract data type of a predicate.
--   Most users will want the type variable to be a member of 'PropLit'


(??\/) :: PropLit a => a -> a -> Reduce a
a ??\/ b | disableSimplify = None

         | [(a,True)] ?=> b == Just True = Value b
{-
a b | a => b | a v b | a v b == b
F F     T        F        T
F T     T        T        T
T F     F        T        ?
T T     T        T        T 
-}

         | [(b,True)] ?=> a == Just True = Value a
{- reverse of above -}

         | otherwise = a ?\/ b


(??/\) :: PropLit a => a -> a -> Reduce a
a ??/\ b | disableSimplify = None

         | a_implies_b == Just True  = Value a
         | a_implies_b == Just False = Literal False
         | b_implies_a == Just True  = Value b
         | b_implies_a == Just False = Literal False
{-
a b | a => b | a ^ b | a ^ b == a
F F     T        F        T
F T     T        F        T
T F     F        F        ?
T T     T        T        T

a b | a => ¬b | a ^ b | a ^ b == F
F F      T        F        T
F T      T        F        T
T F      T        F        T
T T      F        T        ?
-}

         | otherwise = a ?/\ b
    where
        a_implies_b = [(a,True)] ?=> b
        b_implies_a = [(b,True)] ?=> a


reduceList :: PropLit a => (a -> a -> Reduce a) -> [a] -> [Formula a]
reduceList pair xs = f xs
    where
        f xs = g Nothing xs (allPairs xs)
        
        
        g Nothing orig [] = map simpItem orig
        g (Just (_,res)) orig [] = res
        g pending orig (((a,b),rest):remainder) =
            case pair a b of
                None -> g pending orig remainder
                Value x -> f (x:rest)
                Literal b -> predBool b : f rest

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


simpItem :: PropLit a => a -> Formula a
simpItem x = case simp x of
                  Just b | not disableSimplify -> predBool b
                  _ -> predLit x


-- * Simple tests and extractors

-- | return a list of the items, if the item is not a 'predAnd' its the singleton list
fromAnd :: Formula a -> [Formula a]
fromAnd (And x) = if null x then [predTrue] else x
fromAnd x           = [x]


-- | return a list of the items, if the item is not a 'predOr' its the singleton list
fromOr :: Formula a -> [Formula a]
fromOr (Or x) = if null x then [predFalse] else x
fromOr x          = [x]



-- * Creators


-- | A constant True
predTrue :: Formula a
predTrue = And []

-- | A constant False
predFalse :: Formula a
predFalse = Or []

-- | Create a predicate that is just a literal
predLit :: a -> Formula a
predLit x = Lit x

-- | Crashes if 'isLit' is False
fromLit :: Formula a -> a
fromLit (Lit x) = x


solveTerms f items = lits2 ++ terms
    where
        lits2 = reduceList f (map fromLit lits)
        (lits, terms) = partition isLit items


-- | Combine a list of predicates with and
predAnd :: PropLit a => [Formula a] -> Formula a
predAnd xs = case items of
                [x] -> x
                xs | any isFalse xs -> predFalse
                   | otherwise -> And xs
    where
        items = nub $ filter (not . isTrue) $ solveTerms (??/\) $ concatMap fromAnd xs


-- | Combine a list of predicates with or
predOr :: PropLit a => [Formula a] -> Formula a
predOr xs = case items of
                [x] -> x
                xs | any isTrue xs -> predTrue
                   | otherwise -> Or xs
    where
        items = nub $ filter (not . isFalse) $ solveTerms (??\/) $ concatMap fromOr xs


-- | Return True only if the predicate is definately False. Note that predFalse /is not/ not . predTrue
isFalse :: Formula a -> Bool
isFalse (Or  []) = True
isFalse (And xs) = any isFalse xs
isFalse (Or  xs) = all isFalse xs
isFalse _ = False

-- | Return True only if the predicate is definately True
isTrue :: Formula a -> Bool
isTrue (And []) = True
isTrue (And xs) = all isTrue xs
isTrue (Or  xs) = any isTrue xs
isTrue _ = False


-- | Is a predicate just a literal
isLit :: Formula a -> Bool
isLit (Lit x) = True
isLit _ = False


-- | Create a predicate that matches the boolean value
predBool :: Bool -> Formula a
predBool True  = predTrue
predBool False = predFalse


-- | Negate a predicate
predNot :: PropLit a => Formula a -> Formula a
predNot x =
    case x of
        Or  xs -> predAnd $ map predNot xs
        And xs -> predOr  $ map predNot xs
        Not x  -> Lit x
        Lit x  -> maybe (Not x) Lit (litNot x)


-- * Show

-- | Show a predicate nicely
showPred :: Show a => Formula a -> String
showPred x = showPredBy show x


-- | Show a predicate, with a special function for showing each element
showPredBy :: (a -> String) -> Formula a -> String
showPredBy f x =
    case x of
        Or  [] -> "False"
        And [] -> "True"
        Not  a -> "~" ++ f a
        Lit  a -> f a
        Or  xs -> disp 'v' xs
        And xs -> disp '^' xs
    where
        disp sym xs = "(" ++ mid ++ ")"
            where mid = concat $ intersperse [' ',sym,' '] $ map (showPredBy f) xs

instance Show a => Show (Formula a) where
    show = showPredBy show

-- * Eq

instance Eq a => Eq (Formula a) where
    (Lit a) == (Lit b) = a == b
    (And a) == (And b) = sameSet a b
    (Or  a) == (Or  b) = sameSet a b
    _ == _ = False


sameSet a b | length a /= length b = False
            | otherwise = f [] a b
    where
        f [] [] [] = True
        f acc (a:as) (b:bs) | a == b = f [] as (acc ++ bs)
                            | otherwise = f (b:acc) (a:as) bs
        f _ _ _ = False                        


-- * Maps and traversals

-- | Perform a map over every literal
mapPropLit :: PropLit b => (a -> Formula b) -> Formula a -> Formula b
mapPropLit f x =
    case x of
        Or  xs -> predOr  $ fs xs
        And xs -> predAnd $ fs xs
        Lit x  -> f x
    where
        fs = map (mapPropLit f)

-- | Perform a map over every literal, mondically
mapPropLitM :: (Monad m, PropLit b) => (a -> m (Formula b)) -> Formula a -> m (Formula b)
mapPropLitM f x =
    case x of
        Or  xs -> liftM predOr  $ fs xs
        And xs -> liftM predAnd $ fs xs
        Lit x  -> f x
        Not x  -> liftM predNot $ f x
    where
        fs = mapM (mapPropLitM f)


-- | Get all literals in a predicate
allPropLit :: Formula a -> [a]
allPropLit x =
    case x of
        Or  xs -> fs xs
        And xs -> fs xs
        Lit x  -> [x]
    where
        fs = concatMap allPropLit


-- | or then and
foldPred :: ([a] -> a) -> ([a] -> a) -> (b -> a) -> Formula b -> a
foldPred for fand fone x =
    case x of
        Or  x -> for  $ f x
        And x -> fand $ f x
        Lit x -> fone x
    where
        f = map (foldPred for fand fone)


foldFormula :: PropFold a res -> Formula a -> res
foldFormula fs x =
    case x of
        Or  x -> foldOr  fs (map (foldFormula fs) x)
        And x -> foldAnd fs (map (foldFormula fs) x)
        Lit x -> foldLit fs x
        Not x -> foldNot fs (foldLit fs x)


{-

-- | convert to DNF
predDnf :: PropLit a => Formula a -> Formula a
predDnf (PropLit x) = PropLit x
predDnf (Or  x) = predOr $ map predDnf x
predDnf (And []) = And []
predDnf (And x) = foldr1 f $ map predDnf x
    where
        f as bs = predOr [predAnd [a,b] | a <- fromOr as, b <- fromOr bs]


predCnf :: PropLit a => Formula a -> Formula a
predCnf = error "todo" -- swapAndOr . predDnf . swapAndOr -- WRONG


swapAndOr :: Formula a -> Formula a
swapAndOr x =
    case x of
        Or  x -> And (map swapAndOr x)
        And x -> Or  (map swapAndOr x)
        PropLit x -> PropLit x
-}
