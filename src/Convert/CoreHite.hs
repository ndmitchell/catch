
module Convert.CoreHite(coreHite) where

import Core
import Hite


coreHite :: Core -> Hite
coreHite (Core xs) = Hite [] (map convFunc xs)

getName (CoreVar x) = x


convFunc :: CoreFunc -> Func
convFunc (CoreFunc (CoreApp name args) body) = Func (getName name) (map getName args) (convExpr body)


convExpr :: CoreExpr -> Expr
convExpr x = f [] x
    where
        f :: [(String, Expr)] -> CoreExpr -> Expr
        f subs y = case y of
                CoreCon x -> Make x []
                CoreApp x xs -> Call (f subs x) (map (f subs) xs)
                CoreInt x -> CallFunc "prim.int"
                CoreChr x -> CallFunc "prim.chr"
                CoreStr x -> CallFunc "prim.str"
                
                CoreVar x -> rep x
                    
                CoreCase _ alts -> Case rSwitch (map g alts)
            where
                rep x = case lookup x subs of
                            Just a -> a
                            Nothing -> Var x ""
                
                CoreCase (CoreVar switch) _ = y
                rSwitch = rep switch
                
                g (CoreVar "_", x) = ("_", f subs x)
                g (CoreApp (CoreCon con) args, x) = (con, f (zipWith h [1..] args ++ subs) x)
                    where h n (CoreVar arg) = (arg, Sel rSwitch (con ++ "_" ++ show n))
