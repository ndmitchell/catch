
module Convert.CoreHite(coreHite) where

import Core
import Hite

import List


coreHite :: Core -> Hite
coreHite (Core xs) = Hite [] (map convFunc xs)

getName (CoreVar x) = x


convFunc :: CoreFunc -> Func
convFunc (CoreFunc (CoreApp name args) body) =
        Func (getName name) (map getName args) (convExpr (map f args) body) Star
    where
        f (CoreVar x) = (x,Var x "") 


convExpr :: [(String, Expr)] -> CoreExpr -> Expr
convExpr subs x = f subs x
    where
        f :: [(String, Expr)] -> CoreExpr -> Expr
        f subs y = case y of
                CoreCon x -> Make x []
                CoreApp x xs -> Call (f subs x) (map (f subs) xs)
                CoreInt x -> CallFunc "prim_int"
                CoreChr x -> CallFunc "prim_chr"
                CoreStr x -> CallFunc "prim_str"
                
                CoreVar x -> rep x
                    
                CoreCase _ _ -> Case rSwitch (map g alts)
            where
                rep x = case lookup x subs of
                            Just a -> a
                            Nothing -> CallFunc x
                
                CoreCase (CoreVar switch) alts = y
                rSwitch = rep switch
                
                g (CoreVar "_", x) = ("_", f subs x)
                g (CoreApp (CoreCon con) args, x) = (con, f (zipWith h [0..] args ++ subs) x)
                    where h n (CoreVar arg) = (arg, Sel rSwitch (getCtor con n))

                getCtor con n = con ++ "$" ++ show n
