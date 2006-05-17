
module Data.Predicate.Char where

import Data.Predicate


data PredChar = PredChar Bool Char
                deriving Show


instance PredLitNot PredChar where
    litNot (PredChar b c) = predLit $ PredChar (not b) c


instance PredLit PredChar where
    (PredChar b1 c1) ?=> (PredChar b2 c2) = b1 == b2 && c1 == c2

    (PredChar b1 c1) ?\/ (PredChar b2 c2) | c1 == c2 && b1 == not b2 = Value True
                                          | otherwise = Same
    
    (PredChar b1 c1) ?/\ (PredChar b2 c2) | c1 == c2 && b1 == not b2 = Value False
                                          | otherwise = Same


instance PredLit Char where
    a ?=> b = a == b
