

module Hite.Reachable(reachable) where

import Hite.Type
import List
import Maybe


reachable :: FuncName -> Hite -> Hite
reachable name hite = hite{funcs = filter g (funcs hite)}
    where
        canFind = fixSet f [name]
        
        g func = funcName func `elem` canFind
        
        f x = concatMap g $ allExpr $ expr $ getFunc x hite
            where
                g (CallFunc x) = [x]
                g _ = []




--fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _    = []
        fix2 f x  done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x

{-
fixSet f [] = []
fixSet f x  = x ++ fixSet f (x2 \\ x)
    where x2 = nub $ concatMap f x

-}
