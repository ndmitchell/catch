
module Data.Proposition.BDD(BDD) where

import Data.Proposition.Internal
import Data.BDD

instance Prop BDD where
    propTrue  = AtomTrue
    propFalse = AtomFalse
    
    propIsTrue  AtomTrue  = True; propIsTrue  _ = False
    propIsFalse AtomFalse = True; propIsFalse _ = False

    propLit a = Choice a AtomFalse AtomTrue

    propAnd = bddAnd
    propOr  = bddOr
    propNot = bddNot

    propMapM = mapBDDM

    propSimplify = bddSimplify (?=>) . bddApplyAnd liftAnd
        where
            liftAnd a b = case a ?/\ b of
                               Value x -> Just x
                               _ -> Nothing
