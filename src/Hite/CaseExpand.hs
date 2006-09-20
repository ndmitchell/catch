
module Hite.CaseExpand(cmd) where

import Hite.Type
import List
import Maybe

cmd = cmdHitePure (const caseExpand) "case-expand"
            "If a case statement is on a variable, make all occurances free of it"


caseExpand :: Hite -> Hite
caseExpand hite = mapExpr f hite
    where
        f (Case on alts) = Case on $ concatMap (g others on) alts
            where others = ctorOthers (getCtor hite $ fst $ head alts) \\ map fst alts
        f x = x
        
        g others on ("",rhs) = case others of
                                   [] -> []
                                   [x] -> g others on (x,rhs)
                                   _ -> [("",rhs)]

        g others on (lhs,rhs) = [(lhs,mapExpr (h on val) rhs)]
            where
                Ctor name args _ = rawCtor $ getCtor hite lhs
                val = Make name (map (Sel on) args)
        
        h from to x | x == from = to
                    | otherwise = x

