
-- ensure all functions have at most one argument

module Prepare.OneArg(oneArg) where

import Yhc.Core
import General.General
import Data.Maybe
import Data.List



-- every single function MUST have exactly one argument
-- functions with single arguments remain as is
-- those with 0 arguments are passed #, and take one arg
-- those with n arguments are passed #n x y z as a tuple
oneArg :: Core -> Core
oneArg core = mapOverCore useTuples $ applyFuncCore mkTuples $ core{coreDatas = newDatas ++ coreDatas core}
    where
        newDatas = map g $ snub [arity | func <- coreFuncs core, let arity = length $ coreFuncArgs func, arity /= 1]
            where
                g n = CoreData name frees [CoreCtor name $ zip frees [Just $ name ++ "_" ++ show i | i <- [1..n]]]
                    where
                        frees = map (:[]) $ take n ['a'..]
                        name = '#' : show n
    
        mkTuples :: CoreFunc -> CoreFunc
        mkTuples (CoreFunc name [x] body) = CoreFunc name [x] body
        mkTuples (CoreFunc name xs body) = CoreFunc name [free] body2
            where
                body2 = if arity == 0 then body else
                        CoreCase (CoreVar free) [(CoreApp (CoreCon ('#':show arity)) (map CoreVar xs), body)]
                arity = length xs
                free = head $ variableSupply 'v' \\ collectAllVars body


        useTuples :: CoreExpr -> CoreExpr
        useTuples (CoreApp (CoreFun x) [y]) = CoreApp (CoreFun x) [y]
        useTuples (CoreApp (CoreFun x) ys) = CoreApp (CoreFun x) [CoreApp (CoreCon $ '#':show arity) ys]
            where arity = length ys
        useTuples x = x
