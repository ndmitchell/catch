
module Constraint.Req(
    Req(..), reduceReq
    ) where

import Constraint.Pred
import RegExp
import List
import Char

import Hite
import General


data Req = Req Expr (RegExp String) [CtorName]

instance Show Req where
    show (Req expr regs opts) =
        "<" ++ inline (show expr) ++ "," ++ show regs ++ "," ++ strSet opts ++ ">"
        where
            inline x = case lines x of
                            [x] -> x
                            xs -> "{" ++ (concat $ intersperse "; " $ map (dropWhile isSpace) xs) ++ "}"


reduceReq (Req a b c) = Req a (reduceRegExp b) c


instance Eq Req where
    (Req a1 b1 c1) == (Req a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
