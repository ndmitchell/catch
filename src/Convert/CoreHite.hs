
module Convert.CoreHite(coreHite) where

import Core
import Hite

import List
import Maybe


coreHite :: Core -> Hite
coreHite (Core xs) = fixData $ Hite (map convData datas) (concatMap convFunc funcs)
    where (datas, funcs) = partition isCoreData xs


isCoreData (CoreData _ _) = True
isCoreData _ = False

getName (CoreVar x) = x
getName x = error $ "Convert.CoreHite.getName: pattern match failure, " ++ show x



allReplace :: [(CoreExpr, CoreExpr)] -> CoreExpr -> CoreExpr
allReplace ren x = mapCore f x
    where
        f x = case lookup x ren of
                  Nothing -> x
                  Just a -> a



letExpand :: CoreExpr -> CoreExpr
letExpand x = mapCore f x
    where
        f (CoreLet (x:xs) y) = f $ allReplace [g x] (CoreLet xs y)
        f (CoreLet []     y) = y
        
--        error $ show (map g x) ++ "\n\n" ++ showCoreExpr y ++ "\n\n" ++ showCoreExpr (allReplace (map g x) y)
        f x = x
        
        g (CoreFunc (CoreApp x []) y) = (x,y)



convData :: CoreItem -> Data
convData (CoreData dname ctors) = Data dname (map f ctors)
    where
        f (CoreCtor cname args) = Ctor cname (zipWith g [1..] args)
            where
                g n (Just x) = x
                g n Nothing = dname ++ "_" ++ cname ++ "_" ++ show n


convFunc :: CoreItem -> [Func]
convFunc (CoreFunc (CoreApp orig_name orig_args) bad_body) =
        if null complexCases then
            [Func name args (convExpr ren body) Star]
        else
            [Func name args newBody Star,
             Func newName allArgs (convExpr ren2 body2) Star]
    where
        body = letExpand bad_body
        name = getName orig_name
        args = map getName orig_args
        newName = name ++ "_CASE"
        newBody =
            Call (CallFunc newName)
            (map (`Var` "") args ++ map (convExpr ren) complexCases)

        ren =  [(x, Var x "") | x <- args]
        ren2 = [(x, Var x "") | x <- allArgs]
        
        body2 = allReplace (zip complexCases (map CoreVar newArgs)) body
        
        newArgs = ["case_" ++ show n | n <- [1..length complexCases]]
        allArgs = args ++ newArgs
        complexCases = nub $ concatMap g (allCore body)
        
        g (CoreCase (CoreVar _) _) = []
        g (CoreCase x _) = [x]
        g _ = []



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
                
                x -> error $ "Convert.CoreHite.convExpr: " ++ show x
            where
                rep x = case lookup x subs of
                            Just a -> a
                            Nothing -> CallFunc x
                
                CoreCase (CoreVar switch) alts = y
                rSwitch = rep switch
                
                g (CoreVar "_", x) = ("_", f subs x)
                g (CoreApp (CoreCon con) args, x) = (con, f (zipWith h [0..] args ++ subs) x)
                    where h n (CoreVar arg) = (arg, Sel rSwitch (getCtor con n))
                g (CoreCon con, x) = g (CoreApp (CoreCon con) [], x)
                   
                g x = error $ "Convert.CoreHite.g: " ++ show x

                getCtor con n = con ++ "$" ++ show n
