
module Constraint.Constraint(
    Reqs, mapReq, allReq
    ) where

import Constraint.Pred
import Constraint.Req


type Reqs = Pred Req



mapReq f x = mapPred g x
    where
        g (PredAtom a) = f a
        g a = a

allReq x = concatMap isReq (allPred x)
    where
        isReq (PredAtom x) = [x]
        isReq _ = []
