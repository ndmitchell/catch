
module Data.Proposition.Fix(PropFix, propRebuildFix) where

import Data.Proposition.Internal
import Data.Proposition.Formula
import Data.Proposition.BDD
import Control.Monad


propRebuildFix :: (Prop p, PropLit a) => p a -> PropFix a
propRebuildFix = propRebuild


data PropFix a = PropFix {fix :: Formula a}


instance Show a => Show (PropFix a) where
    show = show . fix


instance PropLit a => Eq (PropFix a) where
    a == b = f a == f b
        where f = propSimplify . propRebuildBDD . propSimplify . fix


instance Prop PropFix where
    propTrue   = PropFix propTrue
    propFalse  = PropFix propFalse
    propIsTrue  = propIsTrue  . fix
    propIsFalse = propIsFalse . fix
    
    propLit = PropFix . propLit
    propAnd (PropFix a) (PropFix b) = PropFix (propAnd a b)
    propOr  (PropFix a) (PropFix b) = PropFix (propOr  a b)
    propNot (PropFix a) = PropFix (propNot a)

    propMapM f (PropFix a) = liftM PropFix $ propMapM (liftM fix . f) a
    
    propFold (PropFold foldOr foldAnd foldNot foldLit) (PropFix x) = propFold fold2 x
        where fold2 = error "FixProp.propFold, todo"
    
    propSimplify = PropFix . {-propSimplify . propRebuildFormula . propSimplify . propRebuildBDD . -} propSimplify . fix
