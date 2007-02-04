
{- |
Tasks:

Annotate all CoreCtor's with full field names


Replace all Core bound variables with dotted names

case x of
    (:) x.hd x.tl
-}

module Prepare(prepare) where

import Yhc.Core
import General
import Data.Maybe
import Data.List


prepare :: Core -> Core
prepare core = singleArg $ applyBodyCore (rename . dottedVar core2) core2
    where core2 = applyCtorCore prepareCtor $ shorterData core



-- every single function MUST have exactly one argument
-- functions with single arguments remain as is
-- those with 0 arguments are passed #, and take one arg
-- those with n arguments are passed #n x y z as a tuple
singleArg :: Core -> Core
singleArg core = mapOverCore useTuples $ applyFuncCore mkTuples $ core{coreDatas = newDatas ++ coreDatas core}
    where
        newDatas = map g $ snub [arity | func <- coreFuncs core, let arity = length $ coreFuncArgs func, arity /= 1]
            where
                g n = CoreData name frees [CoreCtor name $ zip frees [Just $ name ++ "_" ++ show i | i <- [1..n]]]
                    where
                        frees = map (:[]) $ take n ['a'..]
                        name = '#' : show n
    
        mkTuples :: CoreFunc -> CoreFunc
        mkTuples (CoreFunc name [x] body) = CoreFunc name [x] body
        mkTuples (CoreFunc name xs body) = CoreFunc name [free] $ replaceFreeVars reps body
            where
                reps = zip xs [CoreApp (CoreFun $ ".#" ++ show arity ++ "_" ++ show i) [CoreVar free] | i <- [1..]]
                arity = length xs
                free = head $ variableSupply 'v' \\ collectAllVars body


        useTuples :: CoreExpr -> CoreExpr
        useTuples (CoreApp (CoreFun x) [y]) = CoreApp (CoreFun x) [y]
        useTuples (CoreApp (CoreFun x) ys) = CoreApp (CoreFun x) [CoreApp (CoreCon $ '#':show arity) ys]
            where arity = length ys
        useTuples x = x



altCtors = [(":", ["hd","tl"])
           ,("(,)",["fst","snd"])
           ]


prepareCtor :: CoreCtor -> CoreCtor
prepareCtor (CoreCtor name fields) = CoreCtor name (zipWith f [0..] fields)
    where
        f n (typ,Nothing) = case lookup name altCtors of
                                Just y -> (typ, Just $ y !! n)
                                Nothing -> (typ, Just $ name ++ "_" ++ show n)
        f n x = x


shorterData :: Core -> Core
shorterData = applyCtorCore f . mapOverCore g
    where
        f x = x{coreCtorName = ren $ coreCtorName x}
        
        g (CoreCon x) = CoreCon $ ren x
        g x = x
        
        ren xs = if null b then xs else tail b
            where (a,b) = break (== '.') xs



dottedVar :: Core -> CoreExpr -> CoreExpr
dottedVar core x = mapOverCore f x
    where
        f (CoreCase x alts) = CoreCase x (map (g x) alts)
        f x = x
        
        g x (CoreVar i, rhs) = (CoreVar i, replaceFreeVars [(i, x)] rhs)
        g x (CoreApp (CoreCon name) is, rhs) =
                (CoreCon name, replaceFreeVars (zip (map fromCoreVar is) is2) rhs)
            where is2 = map (\s -> CoreApp (CoreFun ('.':fromJust (snd s))) [x]) $ coreCtorFields $ coreCtor core name


rename :: CoreExpr -> CoreExpr
rename = mapUnderCore f
    where
        f (CorePrim "Prelude.error") = CorePrim "error"
        f x = x
