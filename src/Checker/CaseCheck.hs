
module Checker.CaseCheck(caseCheck) where

import Hite
import General.General
import List
import Maybe
import IO
import Debug.Trace

import Checker.Propagate
import Checker.Backward

import Constraint
import Options



---------------------------------------------------------------------
-- DRIVER

caseCheck :: Handle -> Hite -> IO ()
caseCheck hndl hite = 
    do
        if null res
            then putStrLn "No inexhaustive patterns, your code is safe"
            else do
                putStrLn $ show count ++ " inexhaustive " ++ pattern_s count ++ " found\n"
                ans <- mapM (uncurry $ caseCheckOne hndl) res
                if all isTrue ans
                    then putStrLn "All patterns were shown to be safe"
                    else do
                        let unsafe = length $ filter (not.isTrue) ans
                        putStrLn $ show unsafe ++ " " ++ pattern_s unsafe ++ " are potentially unsafe"
                        putStrLn $ unlines $ ["    " ++ m | ((h,m),b) <- zip res ans, not (isTrue b)]
                        putStrLn $ "Full precondition for safety:"
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


caseCheckOne :: Handle -> Hite -> String -> IO Reqs
caseCheckOne hndl hite msg =
    do
        putStrLn $ "Checking pattern match: " ++ msg
        hPutStrLn hndl $ "\n\n===================================\nChecking pattern match: " ++ msg
        
        let orig = simplifyReqsFull hite $ propagateAll hite "error!" predFalse
        hPutStrLn hndl $ show orig
        res <- reduce hndl hite orig
        let final = simplifyReqsFull hite res
        hPutStrLn hndl $ "=\n" ++ prettyReqs final
        
        if isTrue final
            then putStrLn $ "Safe :-)\n"
            else putStrLn $ "Unsafe :-(, precondition is:\n" ++ prettyReqs final
        
        return final
       


---------------------------------------------------------------------
-- HITE MANIPULATORS



--DEPRECATED
fixBottom :: Hite -> Hite
fixBottom hite = hite{funcs = map f (funcs hite), datas = newData : datas hite}
    where
        newData = Data "Internal" [Ctor "InternalA" [], Ctor "InternalB" []]
        newBody = Case (Make "InternalA" []) [("InternalB", Call (CallFunc "_") [])]
        
        f (Func "error" args body pos) = Func "error" args newBody pos
        f x = x



-- generate all the required Hite instances
-- each instance has exactly one "error!" function
-- and the string (if there is one) corresponds to some information on the parse error
prepareHites :: Hite -> [(Hite, String)]
prepareHites bad_hite = map (\(a,b) -> (Hite (datas hite) a, b)) $ f (funcs hite)
    where
        hite = annotateVar $ fixError $ removeUnderscore bad_hite
        
        f :: [Func] -> [([Func], String)]
        f [] = []
        f (x:xs) = this ++ rest
            where
                this = [(a:xs,b) | (a,b) <- errFuncs x]
                rest = [(x:a,b) | (a,b) <- f xs]


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


-- and annotations as to which function each variable is in
annotateVar :: Hite -> Hite
annotateVar h = mapFunc f h
    where
        f (Func name args body pos) = Func name args (mapExpr (g name) body) pos
        g name (Var x y) = Var x name
        g name x = x


---------------------------------------------------------------------
-- UTILITY FUNCTIONS


simpler x = blurReqsPath (reducePred x)



---------------------------------------------------------------------
-- CORE FUNCTIONS



type Depth = Int

out :: Handle -> Depth -> String -> IO ()
out hndl depth msg = hPutStrLn hndl $ replicate (depth*2) ' ' ++ msg



-- small reduce - only reduce as far as call's
-- or var's or repeat's
-- will take a bounded (hopefully small) amount of time

reduceOneSmall :: Hite -> Req -> Reqs
reduceOneSmall hite x = case x of
        (Req (Var a b) _ _ ) -> predLit x
        (Req (Repeat _ _) _ _) -> predLit x
        (Req (Call _ _) _ _) -> predLit x
        (ReqAll on within) -> predLit $ ReqAll on (reduceManySmall hite within)
        x -> reduceManySmall hite (backward hite x)


reduceManySmall :: Hite -> Reqs -> Reqs
reduceManySmall hite x = mapPredLit (reduceOneSmall hite) x



reduce :: Handle -> Hite -> Reqs -> IO Reqs
reduce hndl hite x = reduceMany hndl hite [] 1 x


reduceOne :: Handle -> Hite -> [Req] -> Bool -> Depth -> Req -> IO Reqs
reduceOne hndl hite pending supress depth orig_req =
    do
        if supress then return () else out hndl depth (show orig_req)
        case orig_req of
            r | r `elem` pending -> do out hndl depth "True -- Pending tied back"
                                       return predTrue

            (Req (Var a b) _ _) -> 
                if b == "*" || b == "main" then
                    return $ predLit orig_req
                else
                    onwards $ propagate hite orig_req

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do x <- reduceMany hndl hite pending depth within
                                     if on == "main" then
                                         return $ mapPredLit starToMain x
                                      else
                                         reduceMany hndl hite (orig_req:pending) depth $ propagateAll hite on x

            r -> onwards $ backward hite r
    where
        onwards = reduceMany hndl hite p2 depth
            where
                p2 = case orig_req of
                    Req _ _ _ -> orig_req : pending
                    _ -> pending

        starToMain (Req on path opts) = predLit $ Req (mapExpr f on) path opts
        f (Var x "*") = Var x "main"
        f x = x


reduceMany :: Handle -> Hite -> [Req] -> Depth -> Reqs -> IO Reqs
reduceMany hndl hite pending depth xs | depth > maxCheckDepth =
    do hPutStrLn hndl "Lazy, giving up (False)"
       return predFalse

reduceMany hndl hite pending depth orig_xs =
        case simp (reduceManySmall hite orig_xs) of
             PredLit x -> reduceOne hndl hite pending False depth x
             x | null (allPredLit x) -> return x
             xs -> f xs
    where
        f xs =
            do
                let reqs = nub $ allPredLit xs
                out hndl depth ("+ " ++ show orig_xs)
                out hndl depth ("  " ++ show xs)
                res <- g xs reqs
                out hndl depth ("- " ++ show res)
                return res

        simp = simplifyMid hite . simpler

        g reqs [] = return reqs
        g reqs (x:xs) = do out hndl (depth+1) ("+ " ++ show x)
                           res <-
                               if x `elem` allPredLit reqs then
                                   do r <- reduceOne hndl hite pending True (depth+2) x
                                      out hndl (depth+1) ("- " ++ show r)
                                      return $ simp $ mapPredLit (replace x r) reqs
                               else
                                   do out hndl (depth+1) ("- ignored for now")
                                      return reqs
                           g res xs

        replace from to x = if x == from then to else predLit x



simplifyMid hite x = if simplifyRegular then simplifyReqs False hite x else x

