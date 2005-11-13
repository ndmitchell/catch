
module CheckCase(checkCase) where


import Hite
import Maybe
import List
import General
import Constraints
import Regex




---------------------------------------------------------------------
-- BINDING DEFINITIONS --

checkCase :: Hite -> FuncName -> String
checkCase hite name = unlines (map ((++) "> " . show) (driver first))

    --error "todo" -- unlines (map show (driver initReqs))
    --unlines (map showPrettys res ++ [result, answer])
    where
        driver x = x : if anyReq x then driver (next x) else []
    
        -- the first condition
        first = simpPredReq (initialPred hite)
        
        -- get the next condition from any condition
        next :: Pred -> Pred
        next x = simpPredReq $ rootClosure hite $ simpPredReq $ mapReq f x
            where
                f req = if reqFunc req == name
                        then Ands [PredFalse ("@main " ++ show req), res]
                        else res
                    where res = propogate hite req
        


type CallStack = [(Req, [Exp])]


---------------------------------------------------------------
-- NEW VERSIONS

backweird :: Hite -> FuncName -> CallStack -> Exp -> Req -> (CallStack, Pred)
-- hite (parentname and condition) exp (condition for exp) -> (recursive calls, precondition)
backweird hite func calls expr req@(Req _ _ path ctors) =
        --if length calls == length (fst res) then res
        --else error $ show $ (length calls, length (fst res), expr)
        --if length calls > 10 then error (show calls) else 
        --debugmsg ("backward", length calls) $
        
        --debugmsg ("[enter backweird on " ++ func ++ " with " ++ show req ++ "]") (debugmsg smsg res)
        --debugmsg ("backward",expr) $
        
        res
    where
        smsg = reverse (reverse msg)
        msg = "[done backweird on " ++ func ++ ", with " ++ show req ++ " gives " ++ show (snd res) ++ "]"
    
        res = case expr of
            Var varId varPath -> (,) calls $ PredReq (Req func varId (varPath `pathJoin` path) ctors)

            Make ctor args ->
                    if isPathEwp path && not (ctor `elem` ctors)
                    then (calls, PredFalse "")
                    else f calls [] (zip args [1..])
                where
                    f c r [] = (c, Ands r)
                    f c r ((expr, n):rest) =
                        case pathMove path (Sel ctor n) of
                           Nothing -> f c r rest
                           Just x -> f c2 (r2:r) rest
                                where (c2, r2) = backweird hite func c expr req{reqPath=x}
            
            Call (CallFunc name) args ->
                    case addStack [] calls of
                        Just x -> (x, PredTrue)
                        Nothing -> mapReqState f calls2 pred3
                where
                    -- try adding it to the call stack, if you succeed return the new call stack
                    addStack done [] = Nothing
                    addStack done (x@(Req parentName _ parentPath parentCtors, recs):xs)
                        | name == parentName && parentPath == path && parentCtors == ctors =
                            Just $ (reverse done) ++ (fst x, expr:recs) : xs
                        | otherwise = addStack (x:done) xs
                    
                    
                    (((_,recs):calls2), pred2) =
                        backweird hite name ((reqMe, []):calls) (getFunc hite name) reqMe
                    reqMe = req{reqFunc=name}
                        
                    pred3 = closure hite recs (simpPredReq pred2)
                
                    f state req =
                        if length args <= reqVar req -1
                        then error $ show $ (name, expr, args, reqVar req - 1)
                        else backweird hite func state (args !! (reqVar req - 1)) req

            Case test opts -> 
                    f calls [] opts
                where
                    f state done [] = (state, Ands done)
                    f state done ((ctor,code):rest) = f s3 (Ors [p1,p2]:done) rest
                        where
                            (s2, p1) = backweird hite func state test req2
                            (s3, p2) = backweird hite func s2    code req

                            req2 = notReq hite $ req{reqPath=lambda, reqCtors=[ctor]}


-----------------------------------------------------------------------------
-- OLD VERSIONS

        
-- \varphi, backward on a function
backwardFunc :: Hite -> FuncName -> Req -> Pred
backwardFunc hite func req@(Req _ _ path ctors) =
       -- (if func == "split" && ctors /= ["Cons"] then \a -> error (show (a,req)) else id) $
        --debugmsgres ("backwardFunc", func,req) $
        closure hite closes (simpPredReq preds)
    where
        (closes, preds) = backward hite req (getFunc hite func) req
        req = Req func 0 path ctors


-- \varphi, backward on an expression
-- no need for fixed pointing
backwardExpr :: Hite -> Exp -> Req -> Pred
backwardExpr hite expr req =
    --debugmsgres ("backwardExpr", expr, req) $
    snd $ backward hite req{reqCtors=["<null>"]} expr req


-- \varphi, backward
backward :: Hite -> Req -> Exp -> Req -> ([Exp], Pred)
-- hite (parentname and condition) exp (condition for exp) -> (recursive calls, precondition)
backward hite parent@(Req func _ parentPath parentCtors) expr req@(Req _ _ path ctors) =
    -- debugmsg ("backward", parent, expr, req) $
    case expr of
        Var varId varPath -> (,) [] $ PredReq (Req func varId (varPath `pathJoin` path) ctors)
        
        Make ctor args ->
                if isPathEwp path && not (ctor `elem` ctors)
                then ([], PredFalse "")
                else (concat (fsts res), Ands (snds res))
            where
                res = catMaybes $ zipWith f args [1..]
                
                f expr n = case pathMove path (Sel ctor n) of
                               Nothing -> Nothing
                               Just x -> Just $ backward hite parent expr req{reqPath = x}
        
        Call (CallFunc name) args ->
                if name == func && parentPath == path && parentCtors == ctors
                then ([expr], PredTrue)
                else mapReqList f (backwardFunc hite name req)
            where
                f req = 
                    if length args <= reqVar req - 1
                    then error $ show $ (name, expr, args, reqVar req - 1)
                    else backward hite parent (args !! (reqVar req - 1)) req
        
        Case test opts -> 
                (concat (fsts res), Ands (snds res))
            where
                res = map f opts
                
                f (ctor, code) = (a1 ++ b1, Ors [a2, b2])
                    where
                        (a1, a2) = backward hite parent test req2
                        (b1, b2) = backward hite parent code req
                        
                        req2 = notReq hite $ req{reqPath=lambda, reqCtors=[ctor]}



-- \psi, propogate
propogate :: Hite -> Req -> Pred
propogate hite req@(Req func num path opts) = Ands $ map g allsus
    where
        allsus = concatMap f (filter (/= func) (fsts $ funcs hite))
        
        g (func, expr) = snd $ backweird hite func [] expr (Req func 0 path opts)
    
        f fun = map ((,) fun) susvars
            where
                susvars = map (\x -> callArgs x !! (num-1)) suscalls
                suscalls = filter (\x -> isCall x && callName (callFunc x) == func) (allExpsFunc hite fun)



data Close = CFwd [Sel]
           | CBwd CtorName [Exp] -- all exp must be var
           | CErr {cerr :: String}
           | CId
           deriving (Eq, Show)

isCErr (CErr _) = True; isCErr _ = False
isCFwd (CFwd _) = True; isCFwd _ = False
isCId x = x == CId

cfwdSel (CFwd x) = x

-- \Delta, closure
closure :: Hite -> [Exp] -> Pred -> Pred
closure hite [] p0 = p0
closure hite xs p0 | p0 == PredTrue = PredTrue
closure hite xs p0 =
--        if func == "append_" then error (show (xs,p0)) else
        
        (

        -- debugmsg ("\n:::{" ++ reverse (reverse (show p0)) ++ "}\n") $
        if any isCErr (snds expToClose)
        then PredFalse $
                --"Failed to close in " ++ func ++
                --" " ++ (cerr $ head $ filter isCErr (snds expToClose))
                error (show ("closure",cerr $ head $ filter isCErr (snds expToClose), xs,p0))
        else fix1 4 p0
        
        )
    where
        func = callName (callFunc (head xs))
        
        -- find all variables in the predicate which require closing
        varToClose = nub $ map reqVar (allReq p0)
        
        -- if you call yourself as a parameter
        allCalls = filter isRec (concatMap allExps xs)
        
        isRec (Call (CallFunc name) args) = name == func
        isRec _ = False
        
        -- a list of variable, closure method
        expToClose = map (\x -> (x, solve x (map (\y -> callArgs y !! (x-1)) allCalls))) varToClose
        
        -- figure out how to close this list
        solve var xs = case nub $ filter (not . isCId) $ map (asClose var) xs of
            []  -> CId
            [x] -> x
            xs | all isCFwd xs -> CFwd (concatMap cfwdSel xs)
            xs  -> CErr ("Many: " ++ show xs)
            
        asClose var (Var var2 [] ) | var == var2 = CId
        asClose var (Var var2 [x]) | var == var2 = CFwd [x]
        
        -- used for reconstitution from a variable
        asClose var (Make name args)
            | args == take (length args) (map (\x -> Var var [Sel name x]) [1..]) = CId
        
        asClose var (Make name args) | all isVar args = CBwd name args
        
        -- calling yourself, hence recursive assumption means its valid
        asClose var x | isRec x = CId
        
        asClose var v2 = CErr $ "Can't close " ++ show var ++ " <- " ++ show v2 ++
            " : " ++ show (simpPredReq p0)
    
    
        -- now the fixed pointing bit
        fix1 0 p = fixInf p0
    
        fix1 n p = if p `predEq` p2 then p else fix1 (n-1) p2
            where p2 = simpPredReq (Ands [p, mapReq next1 p])
        
        fixInf p = if null fails
                   then simpPredReq res
                   else PredFalse $ "Failed to fixInf in " ++ func ++ " with " ++ show fails
            where (fails, res) = mapReqList nextInf p
        
        
        next1 req@(Req func var path opts) =
            case fromJust $ lookup var expToClose of
                
                CId -> PredReq req
                
                CFwd x -> PredReq req{reqPath = x `pathJoinUnion` path}
                
                CBwd name xs -> Ands $
                        (if isPathEwp path && not (name `elem` opts)
                            then [PredFalse "next in closure"]
                            else [] ) ++
                        concat (zipWith f xs [1..])
                    where
                        f (Var v2 pat) n = case pathMove path (Sel name n) of
                            Nothing -> []
                            Just x -> [PredReq req{reqVar = v2, reqPath = pat `pathJoin` x}]

                

        nextInf :: Req -> ([String], Pred)
        nextInf req@(Req func var path opts) = 
            case fromJust $ lookup var expToClose of
                
                CFwd x -> ([], PredReq req{reqPath = rcon [RStar (RUni (map RLit x))] path})
                
                CBwd name xs -> if all isJust conds
                                then ([], Ands (catMaybes (cond1:conds)))
                                else (["error with CBwd " ++ show (name, xs)], PredFalse "")
                    where
                        cond1 = if isPathEwp path && not (name `elem` opts)
                                then Just $ PredFalse "nextInf in closure"
                                else Nothing
                        
                        conds = zipWith f xs [1..]
                        
                        f (Var v2 pat) n | v2 == var =
                            if pat /= [] then error "t1"
                            else if pathMove path (Sel name n) /= Just path then error $ "neq " ++ show (pathMove path (Sel name n), Just path)
                            else Just $ PredReq req
                        
                        f (Var v2 pat) n = case pathMove path (Sel name n) of
                            Nothing -> error "t3"
                            Just x -> if null msg then Just res else error "t4"
                                where
                                    (msg, res) = nextInf req2
                                    req2 = req{reqVar = v2, reqPath = pat `pathJoin` x}
                
                CId -> ([], PredReq req)
                
                x -> error $ "closure.nextInf: " ++ show x



rootCloses :: Hite -> FuncName -> [Exp]
rootCloses hite func = filter f (allExpsFunc hite func)
    where
        f (Call name args) = callName name == func
        f _ = False


rootClosure :: Hite -> Pred -> Pred
rootClosure hite prec = mapReq f prec
    where
        f req = closure hite (rootCloses hite (reqFunc req)) (PredReq req)



-- bootstrap code
-- get the initial precondition

initialPred :: Hite -> Pred
initialPred hite = rootClosure hite (incompleteCases hite)


incompleteCases :: Hite -> Pred
incompleteCases hite = Ands $ map (PredReq . f) lst
    where
        g (name, expr) = map ((,) name) (allExps expr)
        allFuncExps = concatMap g (funcs hite)
        
        lst = filterSnd (\x -> isCase x && not (isCaseComplete hite x)) allFuncExps
        
        f (func, Case (Var var path) opts) = Req func var (asPath path) (fsts opts)

