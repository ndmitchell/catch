
module FixpProp(FixpProp) where

import Data.Proposition
import Control.Monad

data FixpProp a = FixpProp {fixpProp :: Formula a}


instance Show a => Show (FixpProp a) where
    show = show . fixpProp


instance PropLit a => Eq (FixpProp a) where
    a == b = f a == f b
        where f = propSimplify . propRebuildBDD . propSimplify . fixpProp


instance Prop FixpProp where
    propTrue   = FixpProp propTrue
    propFalse  = FixpProp propFalse
    propIsTrue  = propIsTrue  . fixpProp
    propIsFalse = propIsFalse . fixpProp
    
    propLit = FixpProp . propLit
    propAnd (FixpProp a) (FixpProp b) = FixpProp (propAnd a b)
    propOr  (FixpProp a) (FixpProp b) = FixpProp (propOr  a b)
    propNot (FixpProp a) = FixpProp (propNot a)

    propMapM f (FixpProp a) = liftM FixpProp $ propMapM (liftM fixpProp . f) a
    
    propFold (PropFold foldOr foldAnd foldNot foldLit) (FixpProp x) = propFold fold2 x
        where fold2 = error "FixProp.propFold, todo"
    
    propSimplify = FixpProp . propSimplify . propRebuildFormula . propSimplify . propRebuildBDD . propSimplify . fixpProp
