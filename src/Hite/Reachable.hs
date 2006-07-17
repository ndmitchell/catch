

module Hite.Reachable(cmd, reachable, reachableList) where

import Hite.Type
import List
import Maybe
import General.General
import Data.Predicate


cmd = cmdHitePure reachable "reachable"
            "Reachability analysis, from a root function (default=main) figure out which functions can be used"



reachable :: FuncName -> Hite -> Hite
reachable ""   hite = reachableList ["main"] hite
reachable name hite = reachableList [name]   hite


reachableList :: [FuncName] -> Hite -> Hite
reachableList names hite@(Hite datas funcs) = Hite aliveDatas aliveFuncs
    where
        aliveFuncNames = fixSet f names
        aliveFuncs = [x | x <- funcs, funcName x `elem` aliveFuncNames]

        -- still misses data which is only used as a field selector        
        aliveCtorNames = nub $ concatMap g $ concatMap (allExpr . body) aliveFuncs
        
        g (Make x _) = [x]
        g (Case _ alts) = fsts alts
        g (Sel _ path) = [ctorName $ getCtorFromArg hite path]
        g (Msg x) = map charCtor x
        g (MCase xs) = concat [ctor : concatMap g (allExpr x2) | MCaseAlt x y <- xs, MCaseOpt x2 ctor <- allPredLit x]
        g _ = []
        
        aliveDatas = concatMap h datas
        
        h (Data x xs) = if null xs2 then []
                        else if x == "%Ap" then [Data x xs2]
                        else if length xs - length xs2 <= 1 then [Data x xs]
                        else [Data x (Ctor (x ++ "_?") [] : xs2)]
            where xs2 = filter (\y -> ctorName y `elem` aliveCtorNames) xs
        
        f x = [y | CallFunc y <- allExpr $ body $ getFunc hite x]

