
module Checker.CaseCheck(caseCheck) where

import qualified Checker.Solver

import IO
import Hite
import Constraint
import General.General


caseCheck :: Handle -> Hite -> IO ()
caseCheck hndl hite = --error $ show $ initErrors $ removeError hite

     Checker.Solver.caseCheck hndl hite



removeError :: Hite -> Hite
removeError hite = hite{funcs = filter ((/=) "error" . funcName) (funcs hite)}


initErrors :: Hite -> [(String, Reqs)]
initErrors hite = [(head errors, condToPred cond) |
        func <- funcs hite, let MCase alts = body func,
        MCaseAlt cond val <- alts, let errors = getErrors val, not (null errors)]
    where
        getErrors val = [getMsg args | Call (CallFunc "error") args <- allExpr val]
        condToPred cond = predAnd $ map (predLit . f) cond
        
        getMsg [Msg x] = x
        getMsg [x] = output x
        getMsg xs = show xs
        
        f (expr, cond) = Req expr pathLambda [cond]
    
    
    
