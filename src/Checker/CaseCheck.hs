
module Checker.CaseCheck(caseCheck) where

import Checker.Solver

import IO
import Hite
import Constraint
import General.General
import Checker.Backward
import Monad

import General.Output
import General.General


caseCheck :: Handle -> Hite -> IO ()
caseCheck hndl hite = runOutput hndl (caseCheckOut hite)


caseCheckOut :: Hite -> OutputMonad ()
caseCheckOut hite = do
    putBoth "\n== Pattern Match Checker"
    if null errs then
        putBoth "There are no incomplete cases, safe"
     else do
        putBoth $ show lerrs ++ " incomplete case" ++ ['s'|lerrs/=1] ++ " found\n"
        res <- mapM f (zip [1..] errs)
        let ores = filter (not . isTrue) res
            lres = length ores
        if null ores then
            putBoth "All case statements were shown to be safe"
         else do
            putBoth $ "Final postcondition: " ++ show (predAnd ores)
            putBoth $ show lres ++ " potentially unsafe case" ++ ['s'|lres/=1] ++ " found"
    where
        errs = initErrors $ removeError hite
        lerrs = length errs
        
        f (n, (func, msg, reqs)) = do
            putBoth $ "Checking case [" ++ show n ++ "/" ++ show lerrs ++ "] in function " ++ func
            putBoth $ "    " ++ msg
            res <- solveError hite func reqs
            putBoth ""
            return res
            
            

--     Checker.Solver.caseCheck hndl hite



removeError :: Hite -> Hite
removeError hite = hite{funcs = filter ((/=) "error" . funcName) (funcs hite)}


initErrors :: Hite -> [(FuncName, String, Reqs)]
initErrors hite = [(
                fName,
                headNote "CaseCheck.initErrors" errors,
                predLit $ ReqAll fName $ predNot hite $ mcasePred hite cond) |
        func <- funcs hite, let MCase alts = body func, let fName = funcName func,
        MCaseAlt cond val <- alts, let errors = getErrors val, not (null errors)]
    where
        getErrors val = [getMsg args | Call (CallFunc "error") args <- allExpr val]
        
        getMsg [Msg x] = x
        getMsg [x] = output x
        getMsg xs = show xs
        
        {-
        condToPred funcName cond = predLit $ ReqAll funcName $ mapPredLit g $ predAnd $ map f cond
        g (Req (Var a) c d) = predLit $ Req (Var a) c d
        
        f (expr, cond) = backwardRepeat hite $ Req expr pathLambda (getOtherCtors hite cond)
        -}
    

    
solveError :: Hite -> FuncName -> Reqs -> OutputMonad Reqs
solveError hite func reqs = do
    res <- solve hite ["main"] reqs
    let success = isTrue res
    when (not success) $ putBoth $ prettyReqs res
    putBoth $ if success then "Safe" else "Unsafe"
    return res

