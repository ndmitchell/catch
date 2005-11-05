

module Hite.Reachable(reachable) where

import Hite.Type
import List
import Maybe
import General


reachable :: FuncName -> Hite -> Hite
reachable name hite@(Hite datas funcs) = Hite aliveDatas aliveFuncs
    where
        aliveFuncNames = fixSet f [name]
        aliveFuncs = [x | x <- funcs, funcName x `elem` aliveFuncNames]
        
        aliveCtorNames = nub $ concat [fsts alts | y <- aliveFuncs, Case _ alts <- allExpr (expr y)]
        aliveDatas = [x | x <- datas, any (`elem` aliveCtorNames) (map ctorName (ctors x))]
        
        f x = [y | CallFunc y <- allExpr $ expr $ getFunc x hite]




-- find the fixed point of a set
fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _    = []
        fix2 f x  done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x
