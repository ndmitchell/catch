
module Constraint.Req(
    Req(..)
    ) where

import Constraint.Pred
import Constraint.RegExp

import Hite
import General


data Req = Req FuncName Expr (RegExp String) [CtorName]

instance Show Req where
    show (Req func expr _ opts) = func ++ "@" ++ show expr ++ strSet opts
