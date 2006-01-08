
module Hite.Normalise(normalise) where

import Hite.Type

normalise :: Hite -> Hite
normalise hite = mapExpr f hite
    where
        f (Call x []) = x
        f (Call (Make x []) xs) = Make x xs
        f x = x
