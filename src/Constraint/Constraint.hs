
module Constraint.Constraint(
    Reqs, mapReq, allReq, mapReqM
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


mapReqM f x = mapPredM g x
    where
        g (PredAtom a) = f a
        g a = return a
