
module Hite.ErrorFail(errorFail) where

import Hite.Type

errorFail :: Hite -> Hite
errorFail a = mapExprHite f a
    where
        f (Case a bs) = Case a (filter g bs)
        f x = x
        
        g (_, Call (CallFunc "error") _) = False
        g _ = True
