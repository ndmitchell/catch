
module Checker.Solver(solve) where

import Hite
import General.General
import List
import Maybe
import IO
import Monad
import Debug.Trace

import Checker.Propagate
import Checker.Backward

import Constraint
import Options
import General.Output


---------------------------------------------------------------------
-- DRIVER


-- | Take a Hite program, a set of functions that are the "boundary"
--   and some requirements, and solve it
--   Return True if its successfully solved, False otherwise
solve :: Hite -> [FuncName] -> Reqs -> OutputMonad Bool
solve hite funcs reqs = {- error $ output hite2 -}  reduce hite2 reqs >>= return . isTrue
    where
        hite2 = annotateVar $ annotateFringe funcs $ fixError $ removeUnderscore hite



{-

solveCore 

                        putStrLn $ prettyReqs (simplifyReqsFull hite (predAnd ans))
    where
        hites = prepareHites hite
        count = length hites
        res = zipWith addMsg [0..] hites
        
        pattern_s n = "pattern" ++ ['s'|n/=1]

        addMsg n (h,s) = (h, ran ++ s)
            where
                ran = "[" ++ replicate (length sc - length sn) ' ' ++ sn ++ "/" ++ sc ++ "] "
                sc = show count
                sn = show (n+1)
-}


{-
caseCheckOne :: Handle -> Hite -> [Func] -> String -> IO Reqs
caseCheckOne hndl hite funs msg =
    do
        putStrLn $ "Checking pattern match: " ++ msg
        hPutStrLn hndl $ "\n\n===================================\nChecking pattern match: " ++ msg
        
        let orig = simplifyReqsFull hite $ propagateAll (Hite (datas hite) funs) "error!" predFalse
        hPutStrLn hndl $ show orig
        f funs orig
        
        { -
        res <- reduce hndl hite orig
        
        let final = simplifyReqsFull hite res
        hPutStrLn hndl $ "=\n" ++ prettyReqs final
        
        if isTrue final
            then putStrLn $ "Safe :-)\n"
            else putStrLn $ "Unsafe :-(, precondition is:\n" ++ prettyReqs final
        
        return final
        - }
    where
        -- take a complete component and a requirements, propagate one stage
        f :: [Func] -> Reqs -> IO Reqs
        f funs orig = do
            hPutStrLn hndl $ show orig
            let h2 = Hite (datas hite) (wrapBang hite funs)
            
            res <- reduce hndl h2 orig
            
            -- simplify doesn't understand forall's
            let final = res -- simplifyReqsFull hite res
            hPutStrLn hndl $ "=\n" ++ prettyReqs final
            
            if isTrue final
                then do
                    putStrLn $ "Safe :-)\n"
                    return final
                else do
                    let calls = (findCallers $ allForall final) \\ map funcName funs
                    putStrLn $ prettyReqs final
                    if null (calls \\ ["main"])
                        then do
                            putStrLn $ "Unsafe :-(\n"
                            return final
                        else do
                            hPutStrLn hndl $ "Enlarging with: " ++ show calls
                            f (ensureComplete hite (map (getFunc hite) calls ++ funs)) final


        -- give a list of functions which contain a call to this one
        findCallers :: [FuncName] -> [FuncName]
        findCallers xs = map funcName $ filter hasCall (funcs hite)
            where
                hasCall fun = not $ null [() | CallFunc x <- allExpr fun, x `elem` xs]
       
-}

---------------------------------------------------------------------
-- HITE MANIPULATORS


-- generate all the required Hite instances
-- each instance has exactly one "error!" function
-- and the string (if there is one) corresponds to some information on the parse error
prepareHites :: Hite -> [([Func], String)]
prepareHites bad_hite = concatMap f (funcs hite)
        -- map (\(a,b) -> (Hite (datas hite) a, b)) $ f (funcs hite)
    where
        hite = annotateVar $ fixError $ removeUnderscore bad_hite
        
        {-
        f :: [Func] -> [([Func], String)]
        f [] = []
        f (x:xs) = this ++ rest
            where
                this = [(a:xs,b) | (a,b) <- errFuncs x]
                rest = [(x:a,b) | (a,b) <- f xs]
        -}
        
        f :: Func -> [([Func], String)]
        f func = [(ensureComplete hite [f2], m2) | (f2,m2) <- errFuncs func]
        

        errFuncs :: Func -> [(Func, String)]
        errFuncs func = map f errIds
            where
                expr = body func
                errIds = filterId errCall expr
                
                errCall (Call (CallFunc "error") _) = True
                errCall _ = False
                
                f i = (func{body = newBody}, pos func ++ msg)
                    where
                        newBody = mutateId expr i (Call (CallFunc "error!") [])
                        msg = case callArgs (extractId expr i) of
                                  [Msg x] -> ", " ++ x
                                  _ -> ""




-- remove all _ calls, replace them with _|_ instead
removeUnderscore :: Hite -> Hite
removeUnderscore x = mapExpr f $ x{funcs = filter ((/= "_") . funcName) (funcs x)}
    where
        f (Call (CallFunc "_") []) = Bottom
        f (CallFunc "_") = Bottom
        f x = x


-- make error functions correct
fixError :: Hite -> Hite
fixError hite = hite{funcs = errs ++ filter (not.isError) (funcs hite)}
    where
        isError func = funcName func == "error"
        
        errs = [Func "error" ["x"] Bottom ""
               ,Func "error!" []   Bottom ""
               ]


annotateFringe :: [FuncName] -> Hite -> Hite
annotateFringe fringe hite = hite{funcs = concatMap f (funcs hite)}
    where
        f func@(Func name args body pos) | name `elem` fringe =
            [func, Func ('!':name) args (Call (CallFunc name) (map (`Var` "") args)) pos]
        f x = [x]


-- and annotations as to which function each variable is in
annotateVar :: Hite -> Hite
annotateVar h = mapFunc f h
    where
        f (Func name args body pos) = Func name args (mapExpr (g name) body) pos
        g name (Var x y) = Var x name
        g name x = x

--------------------------------------------------------------------
-- NEW INTERFACE






---------------------------------------------------------------------
-- SCC HITE MANIPULATORS


-- wrap all functions in bang versions
-- should only provide an interface for those functions which
-- are called from the bit of the hite not covered
wrapBang :: Hite -> [Func] -> [Func]
wrapBang hite small = concatMap f small
    where
        inside = map funcName small

        needWrapper = nub $ ["main" | "main" `elem` inside] ++
            [x | ff <- funcs hite, not (funcName ff `elem` inside),
                 CallFunc x <- allExpr ff, x `elem` inside]
    
        f orig | not (funcName orig `elem` needWrapper) = [orig]
    
        f orig@(Func name args body pos) = [orig,
                Func newname args (Call (CallFunc name) (map (`Var` newname) args)) pos]
            where newname = '!':name



-- make the following list of functions complete
ensureComplete :: Hite -> [Func] -> [Func]
ensureComplete orig small = 
        -- PARTIAL implementation
        small ++ [x | x <- funcs orig, funcName x `notElem` map funcName small]

        {- 
        -- FULL implementation
        
        small ++ f (map funcName small) small
    where
        f done pending = if null depends then [] else newFuncs ++ f (depends++done) newFuncs
            where
                newFuncs = map (getFunc orig) depends
                depends = nub [x | CallFunc x <- allExpr pending] \\ done
        -}



---------------------------------------------------------------------
-- CORE FUNCTIONS


reduce :: Hite -> Reqs -> OutputMonad Reqs
reduce hite x = reduceMany hite [] x


reduceOne :: Hite -> [Req] -> Bool -> Req -> OutputMonad Reqs
reduceOne hite pending supress orig_req =
    do
        when (not supress) $ putLog (show orig_req)
        case orig_req of
            r | r `elem` pending -> do putLog "True -- Pending tied back"
                                       return predTrue

            (Req (Var a b) _ _) -> 
                if b == "*" || headNote "Solver.reduceOne" b == '!' then
                    return $ predLit orig_req
                else
                    onwards $ propagate hite orig_req

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do x <- reduceMany hite pending within
                                     if head on == '!' then
                                         return $ predLit (ReqAll (tail on) within)
                                         --return $ mapPredLit (starToName on) x
                                      else
                                         reduceMany hite (orig_req:pending) $ propagateAll hite on x

            r -> onwards $ backward hite r
    where
        onwards = reduceMany hite p2
            where
                p2 = case orig_req of
                    Req _ _ _ -> orig_req : pending
                    _ -> pending

        -- TODO: is starToMain entirely useless?
        starToName name (Req on path opts) = predLit $ Req (mapExpr (f name) on) path opts
        f name (Var x "*") = Var x name
        f name x = x


reduceMany :: Hite -> [Req] -> Reqs -> OutputMonad Reqs
reduceMany hite pending orig_xs = do
        ind <- getIndent
        if ind > maxCheckDepth
            then do putLog "Lazy, giving up (False)"
                    return predFalse
            else
                case simp (backwardRepeatPred hite orig_xs) of
                     PredLit x -> reduceOne hite pending False x
                     x | null (allPredLit x) -> return x
                     xs -> f xs
    where
        f xs =
            do
                let reqs = nub $ allPredLit xs
                putLog $ "+ " ++ show orig_xs
                putLog $ "  " ++ show xs
                res <- g xs reqs
                putLog $ "- " ++ show res
                return res

        simp = simplifyMid hite . simpler

        g reqs [] = return reqs
        g reqs (x:xs) = do incIndent
                           putLog $ "+ " ++ show x
                           res <-
                               if x `elem` allPredLit reqs then
                                   do incIndent
                                      r <- reduceOne hite pending True x
                                      decIndent
                                      putLog $ "  - " ++ show r
                                      return $ simp $ mapPredLit (replace x r) reqs
                               else
                                   do putLog "  - ignored for now"
                                      return reqs
                           decIndent
                           g res xs

        replace from to x = if x == from then to else predLit x



simplifyMid hite x = if simplifyRegular then simplifyReqsFull hite x else x

simpler x = blurReqsPath (reducePred x)

