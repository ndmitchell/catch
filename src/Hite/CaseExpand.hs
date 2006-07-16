
module Hite.CaseExpand(cmd) where

import Hite.Type
import List
import Maybe

cmd = cmdHitePure (const caseExpand) "case-expand"
            "If a case statement is on a variable, make all occurances free of it"


caseExpand :: Hite -> Hite
caseExpand hite = mapExpr f hite
    where
        f (Case on alts) = Case on $ map (g on) alts
        f x = x
        
        g on (lhs,rhs) = (lhs,mapExpr (h on val) rhs)
            where
                Ctor name args = getCtor hite lhs
                val = Make name (map (Sel on) args)
        
        h from to x | x == from = to
                    | otherwise = x

