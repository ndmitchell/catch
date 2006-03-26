
module Hite.ArityRaise(cmd, arityRaise) where

import Hite.Type
import Hite.Normalise

import General.General
import Maybe


cmd = cmdHitePure (const arityRaise) "arity-raise"
            "Remove point free programming"


-- if a function is being deliberate called in a point free style
-- fix it up properly

arityRaise :: Hite -> Hite
arityRaise bad_hite = hite{funcs = map f (funcs hite)}
    where
        hite = normalise bad_hite
        ars = getArity hite
        
        f func@(Func name args body star) | ar == 0 = func
            | otherwise = Func name (args ++ newArgs) (g body) star
            where
                ar = fromJust (lookup name ars) - length args
                newArgs = ["_new" ++ show x | x <- [1..ar]]
                
                g (Call x xs) = Call x (xs ++ map (`Var` "") newArgs)



getArity :: Hite -> [(FuncName, Int)]
getArity hite = fix f (initArity hite)
    where
        f ars = map (g ars) ars
        g ars (func, arity) = (func, calcArity hite ars (getFunc hite func))



initArity :: Hite -> [(FuncName, Int)]
initArity hite = map f (funcs hite)
    where
        f (Func name args body _) = (name, length args)


calcArity :: Hite -> [(FuncName, Int)] -> Func -> Int
calcArity hite ars func = length (funcArgs func) + f (body func)
    where
        f (Call (CallFunc name) xs) = fromJust (lookup name ars) - length xs
        f _ = 0
