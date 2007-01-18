
module FixpPred(FixpPred) where

import Data.Proposition
import Control.Monad

data FixpPred a = FixpPred {fixpPred :: Formula a}


instance Show a => Show (FixpPred a) where
    show = show . fixpPred


instance PropLit a => Eq (FixpPred a) where
    a == b = f a == f b
        where f = propSimplify . propRebuildBDD . propSimplify . fixpPred


instance Prop FixpPred where
    propTrue   = FixpPred propTrue
    propFalse  = FixpPred propFalse
    propIsTrue  = propIsTrue  . fixpPred
    propIsFalse = propIsFalse . fixpPred
    
    propLit = FixpPred . propLit
    propAnd (FixpPred a) (FixpPred b) = FixpPred (propAnd a b)
    propOr  (FixpPred a) (FixpPred b) = FixpPred (propOr  a b)
    propNot (FixpPred a) = FixpPred (propNot a)

    propMapM f (FixpPred a) = liftM FixpPred $ propMapM (liftM fixpPred . f) a
    
    propFold (PropFold foldOr foldAnd foldNot foldLit) (FixpPred x) = propFold fold2 x
        where fold2 = error "FixProp.propFold, todo"
    
    propSimplify = FixpPred . propSimplify . propRebuildFormula . propSimplify . propRebuildBDD . propSimplify . fixpPred
