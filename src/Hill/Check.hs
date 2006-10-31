
module Hill.Check(cmdsCheck, hillCheck) where

import Hill.Type
import General.General


cmdsCheck = [hillCmdPure "check" (const hillCheck)]


-- you need to ask for functions, constructors and selectors
hillCheck hill = if res then hill else error $ "hillCheck failed"
    where
        res = all checkVar (funcs hill) && h efuns funs && h ectrs ctrs && h esels sels
    
        efuns = map funcName $ funcs hill
        ectrs = [n | Data _ cs _ <- datas hill, Ctor n _ _ <- cs]
        esels = [n | Data _ cs _ <- datas hill, Ctor _ ns _ <- cs, n <- ns]
        
        (funs,ctrs,sels) = unzipMerge f $ allOverHill hill
        
        
        f (Fun x) = ([x], [], [])
        f (Ctr x) = ([], [x], [])
        f (Sel x y) = ([], [], [y])
        f (Call x xs) = f (Fun x)
        f (Make x xs) = f (Ctr x)
        f (Case on alts) = unzipMerge g alts
        f _ = ([],[],[])
        
        g (AltCtr x _) = f (Ctr x)
        g _ = ([],[],[])
        
        unzipMerge f xs = (concat a, concat b, concat c)
            where (a,b,c) = unzip3 $ map f xs
        
        h existing wanted = if null doh then True else error $ "Not found, " ++ show doh
            where doh = filter (not . (`elem` existing)) (snub wanted)




checkVar (Func name args body) = f args body
    where
        f free (Let [] x) = f free x
        f free (Let ((l,r):bs) x) = f free r && f (l:free) (Let bs x)
        f free (Var x) = if x `elem` free then True else error $ "checkVar, " ++ name ++ ", missing " ++ show x
        f free x = all (f free) (getChildren x)
