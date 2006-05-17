{- |
    Solve the question "progressively"
    Start with the minimal fragment, progressively increase it
    The idea is the minimal fragment will fixed point quickly
    Saving _loads_ of time from the rest.
-}

module Checker.Progressive(progressiveSolve) where


import General.General
import Hite
import Constraint
import Checker.Solver
import General.Output
import List




progressiveSolve :: Hite -> ReqAlls -> OutputMonad ReqAlls
progressiveSolve hite reqs = do putBoth $ output reqs
                                solve hite2 reqs >>= f
    where
        fringe = nub $ map reqForall $ allPredLit reqs
        hite2 = annotateFringe fringe $ calcFringe hite fringe


        f :: ReqAlls -> OutputMonad ReqAlls
--        f reqs | True = do putBoth $ "progressiveSolve: " ++ show reqs
--                           error "here!"
--                           return reqs
                        
        
        f reqs | "main" `elem` fringe = do putBoth $ output reqs
                                           return reqs
               | isTrue reqs || isFalse reqs = return reqs

               | otherwise = do putBoth $ output reqs
                                putBoth $ "Increasing fringe from " ++ show fringe ++ " to " ++ show newfringe
                                --error $ show $ increaseFringe hite fringe
                                --error $ show fringe ++ "\n" ++ show newfringe ++ "\n" ++ output hite2
                                --error $ output hite2
                                solve hite2 reqs >>= f
            where
                oldfuncs = map funcName $ funcs $ calcFringe hite fringe
                
                hite2 = safeFuncs $ annotateFringe newfringe $ calcFringe hite newfringe
            
                newfringe = increaseFringe hite fringe
                fringe = nub $ map reqForall $ allPredLit reqs
                
                safeFuncs h = h{funcs = concatMap g (funcs h)}
                g func@(Func name args body pos) | name `elem` fringe = [
                        repoint func{funcName = name ++ "!"},
                        Func name args (blankMCase $ Call (CallFunc $ name ++ "!") (map Var args)) pos]
                    | name `elem` oldfuncs = [repoint func]
                    | otherwise = [func]
                
                repoint x = mapExpr h x
                    where
                        h (CallFunc x) | x `elem` fringe = CallFunc (x ++ "!")
                        h x = x
