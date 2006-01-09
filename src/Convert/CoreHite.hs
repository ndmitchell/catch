
module Convert.CoreHite(coreHite) where

import Core
import Hite

import List
import Maybe


coreHite :: Core -> Hite
coreHite (Core xs) = Hite [] (concatMap convFunc xs)

getName (CoreVar x) = x


convFunc :: CoreFunc -> [Func]
convFunc (CoreFunc (CoreApp orig_name orig_args) body) =
        if null complexCases then
            [Func name args (convExpr ren body) Star]
        else
            [Func name args newBody Star,
             Func newName allArgs (convExpr ren2 body2) Star]
    where
        name = getName orig_name
        args = map getName orig_args
        newName = name ++ "_CASE"
        newBody =
            Call (CallFunc newName)
            (map (`Var` "") args ++ map (convExpr ren) complexCases)

        ren =  [(x, Var x "") | x <- args]
        ren2 = [(x, Var x "") | x <- allArgs]
        
        body2 = mapCore h body
        
        newArgs = ["case_" ++ show n | n <- [1..length complexCases]]
        allArgs = args ++ newArgs
        complexCases = nub $ concatMap g (allCore body)
        
        g (CoreCase (CoreVar _) _) = []
        g (CoreCase x _) = [x]
        g _ = []
        
        renComplex = zip complexCases newArgs 
        
        h x@(CoreCase (CoreVar _) _) = x
        h (CoreCase x alts) = CoreCase (CoreVar $ fromJust $ lookup x renComplex) alts
        h x = x


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
