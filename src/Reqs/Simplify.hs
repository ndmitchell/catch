
module Reqs.Simplify(simplifyReqs, simplifyReq) where

import Reqs.Type
import RegExp.Type
import Pred.Type
import Pred.Simplify

import General.Simplify


simplifyReqs :: Reqs -> Reqs
simplifyReqs x = simplifyPred [] [Rule ruleAnd] x
    where
        ruleAnd (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && c1 == c2 =
            Just (Req a1 (regUnion [b1,b2]) c1)
        ruleAnd _ _ = Nothing



simplifyReq x = x

