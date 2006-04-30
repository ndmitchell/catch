
module Hite.MCase(mcase, cmd) where

import Hite.Type


cmd = cmdHitePure (const mcase) "mcase"
            "Move from case to multicase"


mcase :: Hite -> Hite
mcase (Hite datas funcs) = Hite datas (mapExpr f funcs)
    where
        f (Case (Var var _) alts) = MCase $ concatMap g alts
            where
                g (on,MCase alts) = [MCaseAlt ((var,on):opts) expr | MCaseAlt opts expr <- alts]
                g (on,alt) = [MCaseAlt [(var,on)] alt]
        
        f x = x