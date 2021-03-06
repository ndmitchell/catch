
module Hite.Check(check, cmd) where

import Hite.Type
import Hite.Show
import Pred.Predicate
import List
import General.General


cmd = cmdHitePure f "check"
            "Check that a Hite program is well formed"
    where
        f _ h = if check h then h else error "Check failed"


-- in reality either return True, or crash
check :: Hite -> Bool
check (Hite datas funcs) = 
        unique "CtorName" allCtorName && unique "CtorArg" allCtorArg && unique "FuncName" allFuncName &&
        all checkFunc funcs
    where
        allFuncName = map funcName funcs
        allCtorName = concatMap (map ctorName . ctors) datas
        allCtorArg  = concatMap (concatMap ctorArgs . ctors) datas
    
        checkFunc :: Func -> Bool
        checkFunc (Func name args expr _) = unique ("FuncArgs(" ++ name ++ ")") args && checkExpr expr
            where
                checkExpr (Var x) = x `elm` args
                checkExpr (Sel x path) = checkExpr x && path `elm` allCtorArg
                checkExpr (CallFunc x) = x `elm` allFuncName
                checkExpr (Make name args) = name `elm` allCtorName && all checkExpr args
                checkExpr (Call name args) = all checkExpr (name:args)
                checkExpr (Case on alts) = checkExpr on && all checkAlt alts
                checkExpr (Msg x) = True
                checkExpr (Error x) = True
                checkExpr (MCase alts) = all checkCase alts
                checkExpr (Prim x xs) = all checkExpr xs
                
                checkExpr x = error $ "Disallowed language element, " ++ show x
                
                checkAlt (on, arg) = on `elm` allCtorName && checkExpr arg
                
                checkCase (MCaseAlt cond expr) = all checkCaseAlt (allPredLit cond) && checkExpr expr
                checkCaseAlt (MCaseOpt expr ctor) = checkExpr expr && ctor `elm` allCtorName
        
                elm :: String -> [String] -> Bool
                elm x xs | not (x `elem` xs) = error $ "Name not from recognised set: " ++ show x ++
                                                       " in function " ++ name ++ " " ++ strSet xs
                         | otherwise = True

        

unique :: String -> [String] -> Bool
unique msg (x:xs) | null x = error $ "Blank name (" ++ msg ++ ")"
                  | x `elem` xs = error $ "Repeated name (" ++ msg ++ "): " ++ show x
                  | otherwise = unique msg xs
unique msg [] = True


