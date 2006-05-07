
module Checker.Solver(solve) where

import Hite
import General.General
import List
import Maybe
import IO
import Monad
import Debug.Trace

import Checker.Propagate
import Checker.Backward

import Constraint
import Options
import General.Output


---------------------------------------------------------------------
-- DRIVER


-- | Take a Hite program, a set of functions that are the "boundary"
--   and some requirements, and solve it
--   Return True if its successfully solved, False otherwise
--
--   The "edge" functions are assumed to start with an exclamation mark
--   any function marked as being edge *may not* be called from this fragment
solve :: Hite -> Reqs -> OutputMonad Reqs
solve hite reqs = reduce hite2 reqs
    where
        hite2 = removeUnderscore hite


---------------------------------------------------------------------
-- HITE MANIPULATORS


-- remove all _ calls, replace them with _|_ instead
removeUnderscore :: Hite -> Hite
removeUnderscore x = mapExpr f $ x{funcs = filter ((/= "_") . funcName) (funcs x)}
    where
        f (Call (CallFunc "_") []) = Bottom
        f (CallFunc "_") = Bottom
        f x = x


---------------------------------------------------------------------
-- CORE FUNCTIONS


reduce :: Hite -> Reqs -> OutputMonad Reqs
reduce hite x = reduceMany hite [] x


reduceOne :: Hite -> [Req] -> Bool -> Req -> OutputMonad Reqs
reduceOne hite pending supress orig_req =
    do
        when (not supress) $ putLog (show orig_req)
        case orig_req of
            r | r `elem` pending -> do putLog "True -- Pending tied back"
                                       return predTrue


            (Req (Var a) _ _) -> return $ predLit orig_req

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do incIndent
                                     x <- reduceMany hite pending within
                                     putLog $ show x
                                     decIndent
                                     if headNote "Checker.Solver.reduceOne" on == '!' then
                                         return $ predLit (ReqAll (tail on) within)
                                      else
                                         reduceMany hite (orig_req:pending) $ simplify hite $ propagate hite on x

            r -> onwards $ backward hite r
    where
        onwards x = reduceMany hite p2 x
            where
                p2 = case orig_req of
                    Req _ _ _ -> orig_req : pending
                    _ -> pending


reduceMany :: Hite -> [Req] -> Reqs -> OutputMonad Reqs
reduceMany hite pending orig_xs = do
        putLog $ show orig_xs
        ind <- getIndent
        if ind > maxCheckDepth
            then do putLog "Lazy, giving up (False)"
                    return predFalse
            else do
                case simplify hite (backwardRepeatPred hite orig_xs) of
                     PredLit x -> reduceOne hite pending False x
                     x | null (allPredLit x) -> return x
                     xs -> f xs
    where
        f xs =
            do
                let reqs = nub $ allPredLit xs
                putLog $ "+ " ++ show orig_xs
                putLog $ "  " ++ show xs
                res <- g xs reqs
                putLog $ "- " ++ show res
                return res

        g reqs [] = return reqs
        g reqs (x:xs) = do incIndent
                           putLog $ "+ " ++ show x
                           res <-
                               if x `elem` allPredLit reqs then
                                   do incIndent
                                      r <- reduceOne hite pending True x
                                      decIndent
                                      putLog $ "  - " ++ show r
                                      return $ simplify hite $ mapPredLit (replace x r) reqs
                               else
                                   do putLog "  - ignored for now"
                                      return reqs
                           decIndent
                           g res xs

        replace from to x = if x == from then to else predLit x



simplify hite = simplifyMid hite . simpler


simplifyMid hite x = if simplifyRegular then simplifyReqsFull hite x else x

simpler x = blurReqsPath (reducePred x)

