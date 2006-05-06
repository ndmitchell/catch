
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


caseCheck :: String -> Handle -> Hite -> IO Bool
caseCheck file hndl hite = runOutput hndl $ caseCheckOut hite >>= checkOracle file


caseCheckOut :: Hite -> OutputMonad Reqs
caseCheckOut hite = do
    putBoth "\n== Pattern Match Checker"
    if null errs then do
        putBoth "There are no incomplete cases, safe"
        return predTrue
     else do
        putBoth $ show lerrs ++ " incomplete case" ++ ['s'|lerrs/=1] ++ " found\n"
        res <- mapM f (zip [1..] errs)
        let ores = filter (not . isTrue) res
            lres = length ores
            tres = predAnd ores
        if null ores then
            putBoth "All case statements were shown to be safe"
         else do
            putBoth $ "Final postcondition: " ++ show tres
            putBoth $ show lres ++ " potentially unsafe case" ++ ['s'|lres/=1] ++ " found"
        return tres
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

checkOracle :: String -> Reqs -> OutputMonad Bool
checkOracle file req = do src <- outputIO $ readFile "Example/SafePatterns.txt"
                          let ans = lookup file $ map (break (== ' ')) $ lines src
                          case ans of
                                Nothing -> putBoth "Example not found in Oracle" >> return False
                                Just (' ':x) | x == show req -> putBoth "Example matches Oracle" >> return True
                                             | otherwise -> putBoth "Mismatch to Oracle" >> return False
                          

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
    let hite2 = annotateFringe ["main"] hite
    res <- solve hite2 reqs
    let success = isTrue res
    when (not success) $ putBoth $ prettyReqs res
    putBoth $ if success then "Safe" else "Unsafe"
    return res

