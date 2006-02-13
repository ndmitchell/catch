
module Hite.CaseLift(caseLift) where

import Hite.Type
import List
import Maybe

caseLift :: Hite -> Hite
caseLift hite = mapExpr f hite
    where
        valid = [name | Data _ [Ctor name args] <- datas hite]
        
        f (Case on [(lhs, rhs)]) | lhs `elem` valid = rhs
        f x = x
