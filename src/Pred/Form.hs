
module Pred.Form(cnf, dnf) where

import Pred.Type


dnf :: Eq a => Pred a -> Pred a
dnf = swap . cnf . swap
    where
        swap (PredOr  xs) = PredAnd (map swap xs)
        swap (PredAnd xs) = PredOr  (map swap xs)
        swap x = x


cnf :: Eq a => Pred a -> Pred a
cnf x = mapPred f x
    where
        f (PredOr xs) = predAnd $ map predOr $ permute $ map liftAnd xs
        f x = x
        
        liftAnd (PredAnd xs) = xs
        liftAnd x = [x]


permute :: [[a]] -> [[a]]
permute x = foldr f [[]] x
    where
        f as bs = [a:b | a <- as, b <- bs]

