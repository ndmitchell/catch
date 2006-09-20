
module Hite.CaseExpand(cmd1,cmd2) where

import Hite.Type
import List
import Maybe

cmd1 = cmdHitePure (const $ caseExpand False) "case-expand"
            "If a case statement is on a variable, make all occurances free of it"

cmd2 = cmdHitePure (const $ caseExpand True) "case-saturate"
            "Make all case statements complete"


caseExpand :: Bool -> Hite -> Hite
caseExpand saturate hite = mapExpr (caseSimplify hite) $ mapExpr f hite
    where
        f (Case on alts) = Case on $ concatMap (g others on) alts
            where others = ctorOthers (getCtor hite $ fst $ head alts) \\ map fst alts
        f x = x
        
        g others on ("",rhs) =
                if l == 0 then []
                else if l == 1 || saturate then concatMap (g others on) [(x,rhs) | x <- others]
                else [("",rhs)]
            where l = length others

        g others on (lhs,rhs) = [(lhs,mapExpr (h on val) rhs)]
            where
                Ctor name args _ = rawCtor $ getCtor hite lhs
                val = Make name (map (Sel on) args)
        
        h from to x | x == from = to
                    | otherwise = x


caseSimplify hite (Case (Make x xs) alts) | x `elem` map fst alts = fromJust $ lookup x alts
caseSimplify hite (Sel (Make name args) path) | ctorName x == name = args !! cargPos x
    where x = getCArg hite path
caseSimplify hite x = x
