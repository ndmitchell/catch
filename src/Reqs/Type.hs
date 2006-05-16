
module Reqs.Type(
    Req(..), Reqs, ReqAll(..), ReqAlls
    ) where


import Data.Predicate

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
           deriving Eq
           
data ReqAll = ReqAll {reqForall :: FuncName, reqWithin :: Reqs}
              deriving Eq


type Reqs = Pred Req
type ReqAlls = Pred ReqAll
