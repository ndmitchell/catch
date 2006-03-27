
module Convert.CoreHite(coreHite) where

import Core
import Hite

import List
import Maybe
import Char


coreHite :: Core -> Hite
coreHite (Core xs) = Hite newData (concatMap (convFunc newData) funcs)
    where
        (datas, funcs) = partition isCoreData xs
        newData = map convData datas


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



-- make all case on simple variables
simpleCases :: CoreItem -> [CoreItem]
simpleCases (CoreFunc (CoreApp name args) body) =
        CoreFunc (CoreApp name args) (appPos pos r) : rs
    where
        pos = getPos body
    
        (r,rs) = f args body
    
        -- variables, expr
        f :: [CoreExpr] -> CoreExpr -> (CoreExpr, [CoreItem])
        f vars (CoreCase on opts) | isComplex on =
                (CoreApp newCall (on:vars)
                ,CoreFunc (CoreApp newCall (newArg:vars)) (appPos pos res)
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



convFunc :: [Data] -> CoreItem -> [Func]
convFunc datas (CoreFunc (CoreApp (CoreVar name) args) body) = 
        Func name [x | CoreVar x <- args] res pos : rest
    where
        (res, rest) = f [] (map asVar args) body
        asVar (CoreVar x) = (x, Var x "")
        
        pos = getPosStr body
        
    
        f :: [Int] -> [(String, Expr)] -> CoreExpr -> (Expr, [Func])
        
        -- do the simple ones first
        f path vars (CorePos _ x) = f path vars x
        f path vars (CoreCon x) = (Make x [], [])
        f path vars (CoreInt x) = (CallFunc "prim_int", [])
        f path vars (CoreInteger x) = (CallFunc "prim_int", [])
        f path vars (CoreChr x) = (Make (asChar x) [], [])
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
                    makeNewCall path vars "let" (map getBody topbinds) rename
            where
                bindnames = map getName binds
                (topbinds, botbinds) = partition isTopLevel binds
                
                rename bindargs = mapCore fren (if null botbinds then body else CoreLet botbinds body)
                    where
                        ren = zip (map getName topbinds) bindargs
                        fren (CoreVar x) = case lookup x ren of
                                                Just x -> x
                                                Nothing -> CoreVar x
                        fren x = x
                        
                
                isTopLevel (CoreFunc _ body) = null [x | CoreVar x <- allCore body, x `elem` bindnames]
                getBody (CoreFunc _ body) = body
                getName (CoreFunc (CoreApp (CoreVar name) _) _) = name
            

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
                dealMatch (CoreChr c  ) = (asChar c, [])
                dealMatch (CoreCon con) = dealMatch (CoreApp (CoreCon con) [])
                dealMatch (CoreApp (CoreCon con) args) = (con,
                        [(v, Sel newOn x) | (CoreVar v, x) <- zip args sels])
                    where (Ctor _ sels) = getCtor (Hite datas []) con
                
                dealMatch x = error $ "Convert.CoreHite.dealMatch, " ++ show x
                
                givenCtors = filter (/= "_") $ map (fst . dealMatch . fst) opts
                underCtors = getCtorsFromCtor (Hite datas []) (head givenCtors) \\ givenCtors
                
                g path (lhs, rhs) =
                        if c == "_"
                        then (map (\x -> (x,a)) underCtors, b)
                        else ([(c,a)], b)
                    where
                        (c, r) = dealMatch lhs
                        (a, b) = f path (r++vars) rhs

        f path vars x = error $ "Convert.CoreHite.convFunc.f, " ++ show x

        
        isComplex vars (CoreVar x) | x `elem` map fst vars = False
        isComplex _ _ = True

{-                
                
                
                ("_", f subs x)
                
                g path (CoreApp (CoreCon con) args, x) = (con, f (zipWith h [0..] args ++ subs) x)
                    where h n (CoreVar arg) = (arg, Sel rSwitch (getCtor con n))
                g path (CoreCon con, x) = g (CoreApp (CoreCon con) [], x)
                g path (CoreChr x, y) = (asChar x, f subs y)
                   
                g path x = error $ "Convert.CoreHite.g: " ++ show x

               
               
                       f (Case x alts) = Case x (concatMap g alts)
                           where
                               allCtors = getCtorsFromCtor h (headNote "Hite.Data.fixData" myCtors)
                               myCtors = filter (/= "_") $ map fst alts
                               
                               g ("_", b) = zip (allCtors \\ myCtors) (repeat b)
                               g (a  , b) = [(a,b)]

               
               

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


-}
        
        
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
                
                (newBody, newFuncs) = f (0:path) (map (\x -> (x, Var x "")) allArgs) (body (map CoreVar newArgs))
                
                g x | x < 10 = show x
                    | otherwise = '_' : show x
                
                reqVars = filter (\(n,_) -> n `elem` reqArgs) vars
                reqArgs = [x | Var x "" <- allExpr newBody]
                
        {-
        
        
        f unique vars (CoreCase on opts) | isComplex on =
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

        -}


asChar :: Char -> String
asChar c = "Char_" ++ (if isAlphaNum c then [c] else pad3 (show (ord c)))
    where pad3 x = replicate (3 - length x) '0' ++ x


{-
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


-}
