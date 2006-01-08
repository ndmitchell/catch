
module Constraint.Req(
    Req(..)
    ) where

import Constraint.Pred
import Constraint.RegExp

import Hite
import General


data Req = Req Expr (RegExp String) [CtorName]

instance Show Req where
    show (Req expr regs opts) = show expr ++ "." ++ show regs ++ strSet opts
