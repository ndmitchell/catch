
module Data.Proposition.All(PropAll(..)) where

import Data.Proposition.Internal
import Data.Proposition.Simple
import Data.Proposition.Formula
import Data.Proposition.BDD

import Control.Monad


data PropAll a = PropAll {propSimple :: PropSimple a, propFormula :: Formula a, propBDD :: BDD a}


assertNote msg True  y = y
assertNote msg False y = error $ "Assertion failed: " ++ msg


instance Show a => Show (PropAll a) where
    show = show . propSimple


instance Prop PropAll where
    propTrue  = PropAll propTrue  propTrue  propTrue
    propFalse = PropAll propFalse propFalse propFalse
    
    propIsTrue (PropAll a b c) =
        if propIsTrue a
        then assertNote ("PropAll.propIsTrue" ++ show (a,b,c)) (propIsTrue b && propIsTrue c) True
        else False

    propIsFalse (PropAll a b c) =
        if propIsFalse a
        then assertNote ("PropAll.propIsFalse" ++ show (a,b,c)) (propIsFalse b && propIsFalse c) True
        else False
    
    propLit a = PropAll (propLit a) (propLit a) (propLit a)
    propAnd (PropAll a1 b1 c1) (PropAll a2 b2 c2) = PropAll (propAnd a1 a2) (propAnd b1 b2) (propAnd c1 c2)
    propOr  (PropAll a1 b1 c1) (PropAll a2 b2 c2) = PropAll (propOr  a1 a2) (propOr  b1 b2) (propOr  c1 c2)
    
    propNot (PropAll a b c) = PropAll (propNot a) (propNot b) (propNot c)
    
    propMapM f (PropAll a b c) = do
        a2 <- propMapM (liftM propSimple  . f) a
        b2 <- propMapM (liftM propFormula . f) b
        c2 <- propMapM (liftM propBDD     . f) c
        return $ PropAll a2 b2 c2

    propRebuild = propRebuild . propSimple
