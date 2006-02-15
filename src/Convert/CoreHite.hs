
module Convert.CoreHite(coreHite) where

import Core
import Hite

import List
import Maybe
import Char


coreHite :: Core -> Hite
coreHite (Core xs) = fixData $ Hite (map convData datas) (concatMap convFunc funcs)
    where (datas, funcs) = partition isCoreData xs


getName (CoreVar x) = x
getName x = error $ "Convert.CoreHite.getName: pattern match failure, " ++ show x



allReplace :: [(CoreExpr, CoreExpr)] -> CoreExpr -> CoreExpr
allReplace ren x = mapCore f x
    where
        f x = case lookup x ren of
                  Nothing -> x
                  Just a -> a


-- Perform let expansion
letExpand :: CoreExpr -> CoreExpr
letExpand x = mapCore f x
    where
        f (CoreLet (x:xs) y) = f $ allReplace [g x] (CoreLet xs y)
        f (CoreLet []     y) = y
        f x = x
        
        g (CoreFunc (CoreApp x []) y) = (x,y)



convData :: CoreItem -> Data
convData (CoreData dname ctors) = Data dname (map f ctors)
    where
        f (CoreCtor cname args) = Ctor cname (zipWith g [1..] args)
            where
                g n (Just x) = x
                g n Nothing = dname ++ "_" ++ cname ++ "_" ++ show n


-- make all case on simple variables
simpleCases :: CoreItem -> [CoreItem]
simpleCases (CoreFunc (CoreApp name args) body) =
        CoreFunc (CoreApp name args) r : rs
    where
        (r,rs) = f args body
    
        -- variables, expr
        f :: [CoreExpr] -> CoreExpr -> (CoreExpr, [CoreItem])
        f vars (CoreCase on opts) | isComplex on =
                (CoreApp newCall (on:vars)
                ,CoreFunc (CoreApp newCall (newArg:vars)) res
                :rest)
            where
                (res,rest) = f (newArg:vars) (mapCore g $ CoreCase on opts)
            
                newCall = CoreVar $ getName name ++ "_CASE_" ++ show n
                newArg = CoreVar $ "_case_" ++ show n
                n = fromJust $ lookup on complexCases
                
                g x | x == on = newArg
                    | otherwise = x
        
        f vars (CorePos _ x) = f vars x
        
        f vars (CoreCase on opts) = (CoreCase on (map fst res), concatMap snd res)
            where
                res = map g opts
                
                g (when,body) = ((when,a),b)
                    where (a,b) = f ([x | x@(CoreVar y) <- allCore when, y /= "_"] ++ vars) body
        
        f vars (CoreApp x xs) = (CoreApp x2 xs2, concatMap snd res)
            where
                x2:xs2 = map fst res
                res = map (f vars) (x:xs)
            
        f vars x = (x, [])
        
        
        complexCases = (`zip` [1..]) $ nub [on | CoreCase on _ <-  allCore body, isComplex on]
        
        isComplex (CoreVar _) = False
        isComplex _ = True




convFunc :: CoreItem -> [Func]
convFunc (CoreFunc def body) = map f res
    where
        res = simpleCases (CoreFunc def (letExpand body))
        
        f (CoreFunc (CoreApp name args) body) =
            Func (getName name) (map getName args) (convExpr (map g args) body) Star
            
        g (CoreVar x) = (x, Var x "")



convExpr :: [(String, Expr)] -> CoreExpr -> Expr
convExpr subs x = f subs x
    where
        f :: [(String, Expr)] -> CoreExpr -> Expr
        f subs y = case y of
                CoreCon x -> Make x []
                CoreApp x xs -> Call (f subs x) (map (f subs) xs)
                CoreInt x -> CallFunc "prim_int"
                CoreChr x -> Make (asChar x) []
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
                g (CoreChr x, y) = (asChar x, f subs y)
                   
                g x = error $ "Convert.CoreHite.g: " ++ show x

                getCtor con n = con ++ "$" ++ show n


                asChar c = "Char_" ++ (if isAlphaNum c then [c] else pad3 (show (ord c)))
                
                pad3 x = replicate (3 - length x) '0' ++ x
