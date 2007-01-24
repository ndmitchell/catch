
module Data.Proposition.Fix(Fix, propRebuildFix) where

import Data.Proposition.Internal
import Data.Proposition.Formula
import Data.Proposition.BDD
import Control.Monad


propRebuildFix :: (Prop p, PropLit a) => p a -> Fix a
propRebuildFix = propRebuild


data Fix a = Fix {fix :: Formula a}


instance Show a => Show (Fix a) where
    show = show . fix


instance PropLit a => Eq (Fix a) where
    a == b = f a == f b
        where f = propSimplify . propRebuildBDD . propSimplify . fix


instance Prop Fix where
    propTrue   = Fix propTrue
    propFalse  = Fix propFalse
    propIsTrue  = propIsTrue  . fix
    propIsFalse = propIsFalse . fix
    
    propLit = Fix . propLit
    propAnd (Fix a) (Fix b) = Fix (propAnd a b)
    propOr  (Fix a) (Fix b) = Fix (propOr  a b)
    propNot (Fix a) = Fix (propNot a)

    propMapM f (Fix a) = liftM Fix $ propMapM (liftM fix . f) a
    
    propFold (PropFold foldOr foldAnd foldNot foldLit) (Fix x) = propFold fold2 x
        where fold2 = error "FixProp.propFold, todo"
    
    propSimplify = Fix . propSimplify . propRebuildFormula . propSimplify . propRebuildBDD . propSimplify . fix
