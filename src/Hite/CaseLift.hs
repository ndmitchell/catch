
module Hite.CaseLift(cmd, caseLift) where

import Hite.Type
import List
import Maybe

cmd = cmdHitePure (const caseLift) "case-lift"
            "Lift up single statement case's (unsafe)"


caseLift :: Hite -> Hite
caseLift hite = mapExpr f hite
    where
        valid = [name | Data _ [Ctor name args _] _ <- datas hite]
        
        f (Case on [(lhs, rhs)]) | lhs `elem` valid = rhs
        f x = x
