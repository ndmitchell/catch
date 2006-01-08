
module Constraint.Req(
    Req(..), reduceReq
    ) where

import Constraint.Pred
import RegExp

import Hite
import General
import General.Similar


data Req = Req Expr (RegExp String) [CtorName]

instance Show Req where
    show (Req expr regs opts) = show expr ++ ":" ++ show regs ++ strSet opts


reduceReq (Req a b c) = Req a (reduceRegExp b) c


instance Eq Req where
    (Req a1 b1 c1) == (Req a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
