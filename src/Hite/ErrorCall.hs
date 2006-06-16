
-- If a function is a constant then inline it
-- If a function only calls constant functions, then inline it

module Hite.ErrorCall(cmd) where

import Hite.Type


cmd = cmdHitePure (const errorCall) "error-call"
            "Replaces calls to error with an explicit value"


errorCall :: Hite -> Hite
errorCall hite = mapExpr f hite
    where
        f (Call (CallFunc "error") [Msg x]) = Error x
        f x = x
