
module Constraint.Req(
    Req(..), reduceReq
    ) where

import Constraint.Pred
import Constraint.RegExp

import Hite
import General


data Req = Req Expr (RegExp String) [CtorName]
           deriving Eq

instance Show Req where
    show (Req expr regs opts) = show expr ++ "." ++ show regs ++ strSet opts


reduceReq (Req a b c) = Req a (simp b) c
