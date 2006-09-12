
module Convert.CoreHite(coreHite, mergeHites, coreDatas, coreFuncs) where

import Core
import Hite

import List
import Maybe
import Char
import General.General



mergeHites :: [Hite] -> Hite
mergeHites xs = expandCase $ ensureCtors $ Hite (concatMap datas xs) (concatMap funcs xs)


coreHite :: Core -> Hite
coreHite (Core n d xs) = Hite newData (concatMap (convFunc newData) funcs)
    where
        (datas, funcs) = partition isCoreData xs
        newData = map convData datas


coreDatas :: [CoreItem] -> Hite
coreDatas xs = Hite (map convData xs) []


coreFuncs :: Hite -> [CoreItem] -> Hite
coreFuncs hite xs = Hite [] (concatMap (convFunc (datas hite)) xs)


-- for things like Char and Int, new ctor's may be introduced not in the code
-- add them back in
ensureCtors :: Hite -> Hite
ensureCtors hite@(Hite datas funcs) = Hite (foldr f datas ["Int","Integer","Char"]) funcs
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
convData (CoreData dname typ ctors) = Data dname (g [] ctors) typ
    where
    	g seen [] = []
    	g seen (x:xs) = res : g (args++seen) xs
    		where
    			res@(Ctor _ args _) = f seen x
    
        f seen (CoreCtor cname args) = Ctor cname (zipWith g [1..] $ map snd args) (map (convType . fst) args)
            where
                g n (Just x) = demandUnique seen x
                g n (Nothing) | cname == "Prelude.:" = ["","hd","tl"] !! n
                              | cname == "Prelude.1()" = "tup1_1"
                              | "Prelude.(" `isPrefixOf` cname =
                                        let i = length (filter (==',') cname) + 1
                                        in "tup" ++ show i ++ "_" ++ show n
                              | otherwise = dname ++ "_" ++ cname ++ "_" ++ show n

		demandUnique seen x | x `notElem` seen = x
							| otherwise = head [x2 | i <- [1..], let x2 = x ++ show i, x2 `notElem` seen]


-- hacky, will work a bit
convType :: String -> TyType
convType "" = TyNone
convType ('(':xs) = convType (init xs)
convType x = f (words x)
    where
        f [x] | isLower (head x) = TyFree x
        f (x:xs) = TyCon (g x) (map TyFree xs)
        
        g x | "Prelude."  `isPrefixOf` x = drop 8 x
            | "Preamble." `isPrefixOf` x = drop 9 x
            | otherwise = x


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


repeatName = "Prelude.repeat"

repeatBody :: String -> Expr
repeatBody x = Make "Prelude.:" [Var x, Call (CallFunc repeatName) [Var x]]

cycleName = "Prelude.cycle"

cycleBody :: String -> Expr
cycleBody x = Call (CallFunc "Prelude.++") [Var x, Call (CallFunc cycleName) [Var x]]


primitives =
    -- ones which have mutually recursive let's
    ["Numeric.fromRat'","PreludeAux._floatFromRational","PreludeAux._doubleFromRational"] ++
    -- ones which have a too concrete implementation
    ["System.IO.hGetContents","System.IO.stdin","System.IO.stdout","System.IO.stderr"
    ,"YHC.Internal.unsafePerformIO","System.IO.throwIOError","System.IO.hPutChar"]
            


convFunc :: [Data] -> CoreItem -> [Func]
convFunc datas (CoreFunc (CoreApp (CoreVar name) args) body) = 
        map g $ Func name newargs res pos : rest
    where
        newargs = [x | CoreVar x <- args]
        (res, rest) = case () of
            _ | name == "Prelude.error" -> (Prim "error" (map Var newargs), [])
            _ | name == repeatName -> (repeatBody $ head newargs, [])
            _ | name == cycleName -> (cycleBody $ head newargs, [])
            _ | name `elem` primitives
                -> (Prim name (map Var newargs), [])
            _ -> f [] (map asVar args) body
            
        asVar (CoreVar x) = (x, Var x)
        
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
                        ([(c,a)], b)
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
                
