
module Checker.CaseCheck(caseCheck) where

import Checker.Progressive

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


caseCheckOut :: Hite -> OutputMonad ReqAlls
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
            res <- solveError hite reqs
            putBoth ""
            return res
            
            

--     Checker.Solver.caseCheck hndl hite

checkOracle :: String -> ReqAlls -> OutputMonad Bool
checkOracle file req = do src <- outputIO $ readFile "Example/SafePatterns.txt"
                          let ans = lookup file $ map (break (== ' ')) $ lines src
                          case ans of
                                Nothing -> putBoth "Example not found in Oracle" >> return False
                                Just (' ':x) | x == show req -> putBoth "Example matches Oracle" >> return True
                                             | otherwise -> putBoth "Mismatch to Oracle" >> return False
                          

removeError :: Hite -> Hite
removeError hite = hite{funcs = filter ((/=) "error" . funcName) (funcs hite)}


initErrors :: Hite -> [(FuncName, String, ReqAlls)]
initErrors hite = [(
                fName,
                headNote "CaseCheck.initErrors" errors,
                predLit $ ReqAll fName $ reqsNot hite $ mcasePred hite cond) |
        func <- funcs hite, let MCase alts = body func, let fName = funcName func,
        MCaseAlt cond val <- alts, let errors = getErrors val, not (null errors)]
    where
        getErrors val = [getMsg args | Call (CallFunc "error") args <- allExpr val]
        
        getMsg [Msg x] = x
        getMsg [x] = output x
        getMsg xs = show xs
    

    
solveError :: Hite -> ReqAlls -> OutputMonad ReqAlls
solveError hite reqs = do
    res <- progressiveSolve hite reqs
    putBoth $ show res
    putBoth $ if isTrue res then "Safe" else "Unsafe"
    return res

