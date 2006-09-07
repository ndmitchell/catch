
module Hite.Primitive(cmd) where

import Hite.Type
import Hite.Normalise


cmd = cmdHitePure (const prim) "primitive"
            "Introduce primitive functions"


prim :: Hite -> Hite
prim bad_hite = hite{funcs=map f (funcs hite)}
    where
        hite = normalise bad_hite
        
        f (Func name args (Call (CallFunc "prim") []) ty) =
            Func name args (Prim name (map Var args)) ty

        f x = x