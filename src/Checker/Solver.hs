
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
solve :: Hite -> [FuncName] -> Reqs -> OutputMonad Bool
solve hite funcs reqs = {- error $ output hite2 -}  reduce hite2 reqs >>= return . isTrue
    where
        hite2 = annotateVar $ annotateFringe funcs $ fixError $ removeUnderscore hite


---------------------------------------------------------------------
-- HITE MANIPULATORS


-- remove all _ calls, replace them with _|_ instead
removeUnderscore :: Hite -> Hite
removeUnderscore x = mapExpr f $ x{funcs = filter ((/= "_") . funcName) (funcs x)}
    where
        f (Call (CallFunc "_") []) = Bottom
        f (CallFunc "_") = Bottom
        f x = x


-- make error functions correct
fixError :: Hite -> Hite
fixError hite = hite{funcs = errs ++ filter (not.isError) (funcs hite)}
    where
        isError func = funcName func == "error"
        
        errs = [Func "error" ["x"] Bottom ""
               ,Func "error!" []   Bottom ""
               ]


annotateFringe :: [FuncName] -> Hite -> Hite
annotateFringe fringe hite = hite{funcs = concatMap f (funcs hite)}
    where
        f func@(Func name args body pos) | name `elem` fringe =
            [func, Func ('!':name) args (Call (CallFunc name) (map (`Var` "") args)) pos]
        f x = [x]


-- and annotations as to which function each variable is in
annotateVar :: Hite -> Hite
annotateVar h = mapFunc f h
    where
        f (Func name args body pos) = Func name args (mapExpr (g name) body) pos
        g name (Var x y) = Var x name
        g name x = x


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

            (Req (Var a b) _ _) -> 
                if b == "*" || headNote "Solver.reduceOne" b == '!' then
                    return $ predLit orig_req
                else
                    onwards $ propagate hite orig_req

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do x <- reduceMany hite pending within
                                     if head on == '!' then
                                         return $ predLit (ReqAll (tail on) within)
                                         --return $ mapPredLit (starToName on) x
                                      else
                                         reduceMany hite (orig_req:pending) $ propagateAll hite on x

            r -> onwards $ backward hite r
    where
        onwards = reduceMany hite p2
            where
                p2 = case orig_req of
                    Req _ _ _ -> orig_req : pending
                    _ -> pending

        -- TODO: is starToMain entirely useless?
        starToName name (Req on path opts) = predLit $ Req (mapExpr (f name) on) path opts
        f name (Var x "*") = Var x name
        f name x = x


reduceMany :: Hite -> [Req] -> Reqs -> OutputMonad Reqs
reduceMany hite pending orig_xs = do
        ind <- getIndent
        if ind > maxCheckDepth
            then do putLog "Lazy, giving up (False)"
                    return predFalse
            else
                case simp (backwardRepeatPred hite orig_xs) of
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

        simp = simplifyMid hite . simpler

        g reqs [] = return reqs
        g reqs (x:xs) = do incIndent
                           putLog $ "+ " ++ show x
                           res <-
                               if x `elem` allPredLit reqs then
                                   do incIndent
                                      r <- reduceOne hite pending True x
                                      decIndent
                                      putLog $ "  - " ++ show r
                                      return $ simp $ mapPredLit (replace x r) reqs
                               else
                                   do putLog "  - ignored for now"
                                      return reqs
                           decIndent
                           g res xs

        replace from to x = if x == from then to else predLit x



simplifyMid hite x = if simplifyRegular then simplifyReqsFull hite x else x

simpler x = blurReqsPath (reducePred x)

