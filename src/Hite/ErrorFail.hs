
module Hite.ErrorFail(cmd, errorFail) where

import Hite.Type

cmd = cmdHitePure (const errorFail) "error-fail"
            "Remove case clauses that only fail"


errorFail :: Hite -> Hite
errorFail a = mapExpr f a
    where
        f (Case a bs) = Case a (filter g bs)
        f x = x
        
        g (_, Call (CallFunc "error") _) = False
        g _ = True
