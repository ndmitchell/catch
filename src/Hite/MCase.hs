
module Hite.MCase(mcase, cmd) where

import Hite.Type


cmd = cmdHitePure (const mcase) "mcase"
            "Move from case to multicase"


mcase :: Hite -> Hite
mcase (Hite datas funcs) = Hite datas (ensure $ mapExpr f funcs)
    where
        f (Case var alts) = MCase $ concatMap g alts
            where
                g (on,MCase alts) = [MCaseAlt (MCaseAnd (MCaseLit var on:opts)) expr |
                                                      MCaseAlt (MCaseAnd opts) expr <- alts]
                g (on,alt) = [MCaseAlt (MCaseAnd [MCaseLit var on]) alt]
        
        f x = x
        
        
        ensure x = map g x
            where
                g func = func{body = h (body func)}

                h x@(MCase _) = x
                h x = MCase [MCaseAlt (MCaseAnd []) x]
