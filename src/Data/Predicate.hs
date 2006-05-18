

module Data.Predicate(
    Pred, Reduction(..), PredLit, PredLitNot,
    fromAnd, fromOr, fromLit,
    predTrue, predFalse, predLit, predAnd, predOr, predNot, predBool,
    isFalse, isTrue, isLit,
    showPred, showPredBy,
    mapPredLit, allPredLit
    ) where


import Data.List
import Data.Maybe


-- * Debugging options

disableSimplify :: Bool
disableSimplify = False
    
    
-- * Core Type

data Pred a = PredOr  [Pred a]
            | PredAnd [Pred a]
            | PredLit a
            deriving (Read, Show)


data Reduction a = Same
                 | Single a       -- same as Priority infinite
                 | Value Bool
                 | Priority Int a -- higher is bigger priority


class PredLit a => PredLitNot a where
    litNot :: a -> Pred a


class PredLit a where
    (?=>) :: a -> a -> Bool
    (?\/) :: a -> a -> Reduction a
    (?/\) :: a -> a -> Reduction a
    simp :: a -> Maybe Bool
    
    simp x = Nothing
    x ?=> y = False
    x ?\/ y = Same
    x ?/\ y = Same


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

fromAnd :: Pred a -> [Pred a]
fromAnd (PredAnd x) = x
fromAnd x           = [x]


fromOr :: Pred a -> [Pred a]
fromOr (PredOr x) = x
fromOr x          = [x]



-- * Creators


predTrue = PredAnd []

predFalse = PredOr []

predLit x = PredLit x
fromLit (PredLit x) = x




solveTerms f items = lits2 ++ terms
    where
        lits2 = reduceList f (map fromLit lits)
        (lits, terms) = partition isLit items


predAnd :: PredLit a => [Pred a] -> Pred a
predAnd xs = case items of
                [x] -> x
                xs | any isFalse xs -> predFalse
                   | otherwise -> PredAnd xs
    where
        items = filter (not . isTrue) $ solveTerms (??/\) $ concatMap fromAnd xs


predOr :: PredLit a => [Pred a] -> Pred a
predOr xs = case items of
                [x] -> x
                xs | any isTrue xs -> predTrue
                   | otherwise -> PredOr xs
    where
        items = filter (not . isFalse) $ solveTerms (??\/) $ concatMap fromOr xs




isFalse (PredOr  []) = True
isFalse (PredAnd xs) = any isFalse xs
isFalse (PredOr  xs) = all isFalse xs
isFalse _ = False

isTrue (PredAnd []) = True
isTrue (PredAnd xs) = all isTrue xs
isTrue (PredOr  xs) = any isTrue xs
isTrue _ = False


isLit (PredLit x) = True
isLit _ = False


predBool True  = predTrue
predBool False = predFalse


predNot :: PredLitNot a => Pred a -> Pred a
predNot x =
    case x of
        PredOr  xs -> predAnd $ map predNot xs
        PredAnd xs -> predOr  $ map predNot xs
        PredLit x  -> litNot x


-- * Show

showPred :: Show a => Pred a -> String
showPred x = showPredBy show x


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

mapPredLit :: (PredLit a, PredLit b) => (a -> Pred b) -> Pred a -> Pred b
mapPredLit f x =
    case x of
        PredOr  xs -> predOr  $ fs xs
        PredAnd xs -> predAnd $ fs xs
        PredLit x  -> f x
    where
        fs = map (mapPredLit f)

allPredLit :: Pred a -> [a]
allPredLit x =
    case x of
        PredOr  xs -> fs xs
        PredAnd xs -> fs xs
        PredLit x  -> [x]
    where
        fs = concatMap allPredLit
        