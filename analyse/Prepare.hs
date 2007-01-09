
{- |
Tasks:

Annotate all CoreCtor's with full field names


Replace all Core bound variables with dotted names

case x of
    (:) x.hd x.tl
-}

module Prepare(prepare) where

import Yhc.Core
import Data.Maybe


prepare :: Core -> Core
prepare core = applyBodyCore (rename . dottedVar core2) core2
    where core2 = applyCtorCore prepareCtor core



altCtors = [("Prelude.:", ["hd","tl"])
           ,("Prelude.(,)",["fst","snd"])
           ]


prepareCtor :: CoreCtor -> CoreCtor
prepareCtor (CoreCtor name fields) = CoreCtor name (zipWith f [0..] fields)
    where
        f n (typ,Nothing) = case lookup name altCtors of
                                Just y -> (typ, Just $ y !! n)
                                Nothing -> (typ, Just $ name ++ "_" ++ show n)
        f n x = x


dottedVar :: Core -> CoreExpr -> CoreExpr
dottedVar core x = mapOverCore f x
    where
        f (CoreCase x alts) = CoreCase x (map (g x) alts)
        f x = x
        
        g x (CoreVar i, rhs) = (x, replaceFreeVars [(i, x)] rhs)
        g x (CoreApp (CoreCon name) is, rhs) =
                (CoreCon name, replaceFreeVars (zip (map fromCoreVar is) is2) rhs)
            where is2 = map (\s -> CoreApp (CoreFun ('.':fromJust (snd s))) [x]) $ coreCtorFields $ coreCtor core name


rename :: CoreExpr -> CoreExpr
rename = mapUnderCore f
    where
        f (CorePrim "Prelude.error") = CorePrim "error"
        f x = x
