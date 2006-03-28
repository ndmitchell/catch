

module Hite.Reachable(cmd, reachable) where

import Hite.Type
import List
import Maybe
import General.General


cmd = cmdHitePure reachable "reachable"
            "Reachability analysis, from a root function (default=main) figure out which functions can be used"


reachable :: FuncName -> Hite -> Hite
reachable name2 hite@(Hite datas funcs) = Hite aliveDatas aliveFuncs
    where
        name = if null name2 then "main" else name2
        
        aliveFuncNames = fixSet f [name]
        aliveFuncs = [x | x <- funcs, funcName x `elem` aliveFuncNames]

        -- still misses data which is only used as a field selector        
        aliveCtorNames = nub $ concatMap g $ concatMap (allExpr . body) aliveFuncs
        
        g (Make x _) = [x]
        g (Case _ alts) = fsts alts
        g (Sel _ path) = [ctorName $ getCtorFromArg hite path]
        g _ = []
        
        aliveDatas = concatMap h datas
        
        h (Data x xs) = if null xs2 then []
                        else if x == "%Ap" then [Data x xs2]
                        else if length xs - length xs2 <= 1 then [Data x xs]
                        else [Data x (Ctor (x ++ "_?") [] : xs2)]
            where xs2 = filter (\y -> ctorName y `elem` aliveCtorNames) xs
        
        f x = [y | CallFunc y <- allExpr $ body $ getFunc hite x]



-- find the fixed point of a set
fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _    = []
        fix2 f x  done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x
