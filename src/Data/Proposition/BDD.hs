
module Data.Proposition.BDD(BDD) where

import Data.Proposition.Internal
import Data.BDD

instance Prop BDD where
	propTrue  = bddTrue
	propFalse = bddFalse
	propIsTrue  = bddIsTrue
	propIsFalse = bddIsFalse
		
	propLit = bddLit
	propAnd = bddAnd
	propOr  = bddOr
	propNot = bddNot litNot

	propMapM = mapBDDM

	propShowBy = showBDDBy
