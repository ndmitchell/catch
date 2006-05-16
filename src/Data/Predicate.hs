

module Data.Predicate where

import Data.List
import Data.Maybe
    
    
-- * Core Type

data Pred a = PredOr  [Pred a]
            | PredAnd [Pred a]
            | PredLit a
            deriving Eq -- can do better than this!!!


class PredLit a where
    litNot :: a -> Pred a
    (?=>) :: a -> a -> Bool
    (?\/) :: a -> a -> Maybe a
    (?/\) :: a -> a -> Maybe a
    simp :: a -> Maybe Bool
    
    simp x = Nothing
    x ?=> y = False
    x ?\/ y = Nothing
    x ?/\ y = Nothing


-- * Useful utilities

(??\/) :: PredLit a => a -> a -> Maybe a
a ??\/ b | a ?=> b = Just b
         | otherwise = a ?\/ b


(??/\) :: PredLit a => a -> a -> Maybe a
a ??/\ b | a ?=> b = Just a
         | otherwise = a ??/\ b


reduceList :: (a -> a -> Maybe a) -> [a] -> [a]
reduceList pair xs = f xs
    where
        f [] = []
        f (x:xs) = g [] x (f xs)
        
        g acc x [] = x:acc
        g acc x (y:ys) | isJust res = g [] (fromJust res) (acc++ys)
                       | otherwise  = g (y:acc) x ys
            where res = pair x y

simpList :: PredLit a => [a] -> [Pred a]
simpList xs = map f xs
    where
        f x = case simp x of
                  Nothing -> predLit x
                  Just b -> predBool b


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
        lits2 = simpList $ reduceList f (map fromLit lits)
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


predNot :: PredLit a => Pred a -> Pred a
predNot x =
    case x of
        PredOr  xs -> predAnd $ map predNot xs
        PredAnd xs -> predOr  $ map predNot xs
        PredLit x  -> litNot x


-- * Show

instance Show a => Show (Pred a) where
    show x = showPred x
    

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
        PredOr  xs -> concatMap allPredLit xs
        PredAnd xs -> concatMap allPredLit xs
        PredLit x  -> [x]
