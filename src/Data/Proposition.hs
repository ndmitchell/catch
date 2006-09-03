
module Data.Proposition(
	module Data.Proposition.Internal,
	module Data.Proposition.BDD, module Data.Proposition.Formula
	) where

import Data.Proposition.BDD
import Data.Proposition.Formula
import Data.Proposition.Internal


instance PropLit Int

value = propLit 12 :: BDD Int

