    
module Hite.Normalise(normalise) where

import Hite.Type

normalise :: PlayExpr a => a -> a
normalise hite = mapExpr f hite
    where
        f (CallFunc x) = Call (CallFunc x) []
        f (Call (Call x xs) ys) = Call x (xs++ys)
        f (Call (CallFunc x) []) = Call (CallFunc x) []
        f (Call x []) = x
        f (Call (Make x []) xs) = Make x xs
        f x = x
