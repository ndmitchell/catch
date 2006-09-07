
module Hite.Primitive(cmd1,cmd2) where

import Hite.Type
import Hite.Normalise
import Data.List


cmd1 = cmdHitePure (const primAdd) "add-primitive"
            "Introduce primitive functions"

cmd2 = cmdHitePure (const primDel) "del-primitive"
            "Remove primitive functions"


primAdd :: Hite -> Hite
primAdd bad_hite = hite{funcs=map f (funcs hite)}
    where
        hite = normalise bad_hite
        
        f (Func name args (Call (CallFunc "prim") []) ty) =
            Func name args (Prim name (map Var args)) ty

        f x = x


primDel :: Hite -> Hite
primDel hite = hite{funcs=map newFunc primsArity ++ mapExpr f (funcs hite)}
    where
        primsArity = sort $ nub [length xs | Prim _ xs <- allExpr hite]
        
        newFunc n = Func ("prim_" ++ show n) ["v" ++ show i | i <- [1..n]] Unknown []
        
        f (Prim x xs) = Call (CallFunc $ "prim_" ++ show (length xs)) xs
        f x = x
