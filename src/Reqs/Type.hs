
module Reqs.Type(
    Req(..), Reqs
    ) where

import Pred.Type
import Pred.Show
import Reqs.Path

import Hite
import General.General
import Char
import List


data Req = Req {reqExpr :: Expr, reqPath :: Path String, reqCtors :: [CtorName], reqIn :: Maybe Expr}
           deriving Eq


type Reqs = Pred Req
