
module Convert.CoreHite(convHite, mergeHites, convDatas, convFuncs) where

import Core
import Hite
import Convert.CoreData

import List
import Maybe
import Char
import General.General


mergeHites :: [Hite] -> Hite
mergeHites xs = reachable "" $ ensureCtors $ reachableCode "" $
                Hite (concatMap datas xs) (concatMap funcs xs)


convHite :: Core -> Hite
convHite (Core n d datas funcs) = Hite newData (concatMap (convFunc newData) funcs)
    where newData = map convData datas


convDatas :: [CoreData] -> Hite
convDatas xs = Hite (map convData xs) []


convFuncs :: Hite -> [CoreFunc] -> Hite
convFuncs hite xs = Hite [] (concatMap (convFunc (datas hite)) xs)


-- for things like Char and Int, new ctor's may be introduced not in the code
-- add them back in
ensureCtors :: Hite -> Hite
ensureCtors hite@(Hite datas funcs) = Hite (foldr f datas ["Int","Integer","Char","Float","Double"]) funcs
    where
        ctors = nub $ concatMap g $ allExpr hite
        
        g (Make name _) = [name]
        g (Msg x) = map charCtor x
        g (Case on alts) = delete "_" $ map fst alts
        g _ = []

        f str datas = map g datas
            where
                g (Data nam crs typ) | nam == str = Data nam [Ctor n [] [] | n <- ctrs] typ
                g x = x
                
                ctrs = nub $ str : filter ((str++"_") `isPrefixOf`) ctors

-- where _ is a case variable, expand it
expandCase :: Hite -> Hite
expandCase hite = mapExpr f hite
    where
        f (Case x xs) = Case x $ concatMap g xs
            where
                seen = delete "_" $ map fst xs 
                rest = ctorNames (getCtor hite (head seen)) \\ seen
        
                g ("_",x) = [(a,x) | a <- rest]
                g x = [x]
        f x = x
        

getName (CoreVar x) = x
getName x = error $ "Convert.CoreHite.getName: pattern match failure, " ++ show x



allReplace :: [(CoreExpr, CoreExpr)] -> CoreExpr -> CoreExpr
allReplace ren x = mapUnderCore f x
    where
        f x = case lookup x ren of
                  Nothing -> x
                  Just a -> a


-- Perform let expansion
letExpand :: CoreExpr -> CoreExpr
letExpand x = mapUnderCore f x
    where
        f (CoreLet (x:xs) y) = f $ allReplace [g x] (CoreLet xs y)
        f (CoreLet []     y) = y
        f x = x
        
        g (x, y) = (CoreVar x,y)



getPos :: CoreExpr -> Maybe String
getPos (CorePos p x) = Just p
getPos _ = Nothing

getPosStr :: CoreExpr -> String
getPosStr x = case getPos x of
                    Just x -> x
                    Nothing -> ""

appPos :: Maybe String -> CoreExpr -> CoreExpr
appPos (Just p) x = CorePos p x
appPos Nothing  x = x


convFunc :: [Data] -> CoreFunc -> [Func]
convFunc datas (CoreFunc name args body) = 
        map g $ Func name args res pos : rest
    where
        (res, rest) = f [] (map asVar args) body
            
        asVar x = (x, Var x)
        
        pos = name ++ ", " ++ getPosStr body
        
        
        g :: Func -> Func
        g (Func name args (Call (CallFunc "primitive") []) res) = Func name args (Prim name (map Var args)) res
        g x = x
        
    
        f :: [Int] -> [(String, Expr)] -> CoreExpr -> (Expr, [Func])
        
        -- do the simple ones first
        f path vars (CorePos _ x) = f path vars x
        f path vars (CoreCon x) = (Make x [], [])
        f path vars (CoreInt x) = (Make (intCtor x) [], [])
        f path vars (CoreInteger x) = (Make (integerCtor x) [], [])
        f path vars (CoreDouble x) = (Make (doubleCtor x) [], [])
        f path vars (CoreFloat x) = (Make (floatCtor x) [], [])
        f path vars (CoreChr x) = (Make (charCtor x) [], [])
        f path vars (CoreStr x) = (Msg x, [])
        
        -- application, need sequencing
        f path vars (CoreApp x xs) = (Call x2 xs2, concatMap snd res)
            where
                x2:xs2 = map fst res
                res = zipWith (\n x -> f (n:path) vars x) [0..] (x:xs)
        
        -- variables, some are local, some are remote
        f path vars (CoreVar x) = case lookup x vars of
                                      Nothing -> (CallFunc x, [])
                                      Just x -> (x, [])

        -- let expressions, expand out
        f path vars (CoreLet binds body) =
                if null topbinds then
                    error $ "Convert.CoreHite, mutually recursive let in " ++ name
                else
                    makeNewCall path vars "let" (map snd topbinds) rename
            where
                bindnames = map fst binds
                (topbinds, botbinds) = partition isTopLevel binds
                
                rename bindargs = mapUnderCore fren (if null botbinds then body else CoreLet botbinds body)
                    where
                        ren = zip (map fst topbinds) bindargs
                        fren (CoreVar x) = case lookup x ren of
                                                Just x -> x
                                                Nothing -> CoreVar x
                        fren x = x
                        
                
                isTopLevel (_, body) = null [x | CoreVar x <- allCore body, x `elem` bindnames]
            

        -- case expressions, always translate, some are complex and need new functions
        f path vars (CoreCase on opts) | isComplex vars on =
                makeNewCall path vars "case" [on] (\[x] -> CoreCase x opts)


        f path vars (CoreCase on opts) = 
                (Case newOn (concatMap fst res), onFuncs ++ concatMap snd res)
            where
                (newOn, onFuncs) = f (0:path) vars on
                res = zipWith g (map (:path) [1..]) opts
                
                -- figure out what the LHS of a case matches
                dealMatch :: CoreExpr -> (CtorName, [(String, Expr)])
                dealMatch (CoreVar "_") = ("_", [])
                dealMatch (CoreChr c  ) = (charCtor c, [])
                dealMatch (CoreInt c  ) = (intCtor c, [])
                dealMatch (CoreCon con) = dealMatch (CoreApp (CoreCon con) [])
                dealMatch (CoreApp (CoreCon con) args) = 
                        if con == "Char_O" then error $ show (CoreApp (CoreCon con) args)
                        else
                        
                        (con,
                        [(v, Sel newOn x) | (CoreVar v, x) <- zip args sels])
                    where sels = ctorArgs $ getCtor (Hite datas []) con
                
                dealMatch x = error $ "Convert.CoreHite.dealMatch, " ++ show x
                
                givenCtors = filter (/= "_") $ map (fst . dealMatch . fst) opts
                underCtors = ctorNames (getCtor (Hite datas []) (head givenCtors)) \\ givenCtors
                
                g path (lhs, rhs) =
                        -- if c == "_"
                        -- then (map (\x -> (x,a)) underCtors, b)
                        -- else
                        ([(if c == "_" then "" else c,a)], b)
                    where
                        (c, r) = dealMatch lhs
                        (a, b) = f path (r++vars) rhs

        f path vars x = error $ "Convert.CoreHite.convFunc.f, " ++ show x

        
        isComplex vars (CoreVar x) | x `elem` map fst vars = False
        isComplex _ _ = True

        
        makeNewCall :: [Int] -> [(String, Expr)] -> String -> [CoreExpr] -> ([CoreExpr] -> CoreExpr) -> (Expr, [Func])
        makeNewCall path vars mode args body = 
                (
                    Call (CallFunc newName) (map fst res ++ map snd reqVars),
                    Func newName (newArgs ++ map fst reqVars) newBody pos : newFuncs ++ concatMap snd res
                )
            where
                res = zipWith (\n x -> f (n:path) vars x) [0..] args
                newPath = concatMap g path ++ "_" ++ mode
                newName = name ++ newPath
                newArgs = map (\n -> newPath ++ "_" ++ show n) [0..length args-1]
                allArgs = newArgs ++ map fst vars
                
                (newBody, newFuncs) = f (0:path) (map (\x -> (x, Var x)) allArgs) (body (map CoreVar newArgs))
                
                g x | x < 10 = show x
                    | otherwise = '_' : show x
                
                reqVars = filter (\(n,_) -> n `elem` reqArgs) vars
                reqArgs = [x | Var x <- allExpr newBody]
                
