
module Hite.Check(check) where

import Hite.Type
import List
import General.General

-- in reality either return True, or crash
check :: Hite -> Bool
check (Hite datas funcs) = 
        unique allCtorName && unique allCtorArg && unique allFuncName &&
        all checkFunc funcs
    where
        allFuncName = map funcName funcs
        allCtorName = concatMap (map ctorName . ctors) datas
        allCtorArg  = concatMap (concatMap ctorArgs . ctors) datas
    
        checkFunc :: Func -> Bool
        checkFunc (Func name args expr kind) = unique args && checkExpr expr
            where
                checkExpr (Var x "") = x `elm` args
                checkExpr (Sel x path) = checkExpr x && path `elm` allCtorArg
                checkExpr (CallFunc x) = x `elm` allFuncName
                checkExpr (Make name args) = name `elm` allCtorName && all checkExpr args
                checkExpr (Call name args) = all checkExpr (name:args)
                checkExpr (Case on alts) = checkExpr on && all checkAlt alts
                
                checkExpr x = error $ "Disallowed language element, " ++ show x
                
                checkAlt (on, arg) = on `elm` allCtorName && checkExpr arg
        
                elm :: String -> [String] -> Bool
                elm x xs | not (x `elem` xs) = error $ "Name not from recognised set: " ++ show x ++
                                                       " in function " ++ name ++ " " ++ strSet xs
                         | otherwise = True

        

unique :: [String] -> Bool
unique (x:xs) | x `elem` xs = error $ "Repeated name: " ++ show x
              | otherwise = unique xs
unique [] = True


