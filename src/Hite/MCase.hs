
module Hite.MCase(mcase, cmd) where

import Hite.Type
import List
import General.General
import Data.Predicate


cmd = cmdHitePure (const mcase) "mcase"
            "Move from case to multicase"


mcase :: Hite -> Hite
mcase (Hite datas funcs) = Hite datas (map factor $ ensure $ mapExpr f funcs)
    where
        f (Case var alts) = MCase $ concatMap g alts
            where
                g (on,MCase alts) = [MCaseAlt (predAnd [predLit (MCaseOpt var on), guard]) expr |
                                                      MCaseAlt guard expr <- alts]
                g (on,alt) = [MCaseAlt (predLit (MCaseOpt var on)) alt]
        
        f x = x
        
        
        ensure x = map g x
            where
                g func = func{body = h (body func)}

                h x@(MCase _) = x
                h x = MCase [MCaseAlt predTrue x]

        
        factor func = func{body = MCase (filter valid res)}
            where
                MCase alts = body func
                res = map rejoin $ groupSetBy splitup alts
                splitup (MCaseAlt a1 b1) (MCaseAlt a2 b2) = b1 == b2
                rejoin xs = MCaseAlt (predOr x) (headNote "Hite.MCase.factor" y)
                    where (x,y) = unzip $ map (\(MCaseAlt a b) -> (a,b)) xs

        valid (MCaseAlt p e) = not (isFalse p)
