
module Convert.CoreHite(coreHite) where

import Core
import Hite


baseTypes :: [Data]
baseTypes = [Data "[]" [Ctor "[]" [], Ctor ":" ["hd","tl"]]]


coreHite :: Core -> Hite
coreHite (Core xs) = Hite baseTypes (map convFunc xs)

getName (CoreVar x) = x


convFunc :: CoreFunc -> Func
convFunc (CoreFunc (CoreApp name args) body) =
        Func (getName name) (map getName args) (convExpr baseTypes (map f args) body)
    where
        f (CoreVar x) = (x,Var x "") 


convExpr :: [Data] -> [(String, Expr)] -> CoreExpr -> Expr
convExpr types subs x = f subs x
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
                            Nothing -> CallFunc x
                
                CoreCase (CoreVar switch) _ = y
                rSwitch = rep switch
                
                g (CoreVar "_", x) = ("_", f subs x)
                g (CoreApp (CoreCon con) args, x) = (con, f (zipWith h [0..] args ++ subs) x)
                    where h n (CoreVar arg) = (arg, Sel rSwitch (getCtor con n))

                getCtor con n = head [xs !! n | Data _ cs <- types, Ctor c xs <- cs, c == con]
