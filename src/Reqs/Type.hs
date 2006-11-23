
module Reqs.Type(
    Req(..), Reqs, ReqAll(..), ReqAlls
    ) where


import Pred.Predicate

{-
import Pred.Type
import Pred.Show
-}

import Reqs.Path

import Hite
import General.General
import Char
import List


data Req = Req {reqExpr :: Expr, reqPath :: Path String, reqCtors :: [CtorName], reqHite :: Hite}
           
data ReqAll = ReqAll {reqForall :: FuncName, reqWithin :: Reqs}


instance Eq Req where
    (Req a1 b1 c1 _) == (Req a2 b2 c2 _) = a1 == a2 && b1 == b2 && (c1 `setEq` c2)
    
instance Eq ReqAll where
    (ReqAll a1 b1) == (ReqAll a2 b2) = a1 == a2 && b1 == b2


type Reqs = Pred Req
type ReqAlls = Pred ReqAll
