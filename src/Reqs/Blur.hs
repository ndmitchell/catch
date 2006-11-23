
module Reqs.Blur() where

import Reqs.Type
import Reqs.Simplify
import General.General
import Pred.Predicate
import Reqs.Path


instance Blur Req where
    blur req = req{reqPath = pathBlur (reqPath req)}

instance Blur ReqAll where
    blur (ReqAll on within) = ReqAll on (blur within)

instance (PredLit a, Eq a, Blur a) => Blur (Pred a) where
    blur x = mapPredLit (predLit . blur) x

