
module Data.Proposition(
	module Data.Proposition.Internal, module Data.Proposition,
	module Data.Proposition.BDD, module Data.Proposition.Formula,
    module Data.Proposition.Simple, module Data.Proposition.All
	) where

import Data.Proposition.BDD
import Data.Proposition.Formula
import Data.Proposition.Simple
import Data.Proposition.All
import Data.Proposition.Internal


propRebuildBDD :: (Prop p, PropLit a) => p a -> BDD a
propRebuildBDD = propRebuild

propRebuildSimple :: (Prop p, PropLit a) => p a -> PropSimple a
propRebuildSimple = propRebuild

propRebuildFormula :: (Prop p, PropLit a) => p a -> Formula a
propRebuildFormula = propRebuild
