
module Checker.CaseCheck(caseCheck) where

import Hite
import General.General
import List
import Maybe

import Checker.Propagate
import Checker.Backward

import Constraint
import Options



---------------------------------------------------------------------
-- DRIVER

caseCheck :: Hite -> IO ()
caseCheck bad_hite = do putStrLn $ "Initial conditions:\n" ++ (unlines $ map ((++) "? " . show) reqs)
                        res <- mapM f reqs
                        let (msg, r) = (map fst res, map snd res)
                        error $ "\n" ++ unlines msg ++
                                "\n? " ++ show (predAnd reqs) ++
                                "\n=\n" ++ prettyReqs (simp $ predAnd r)
    where
        reqs = fromAnd $ propagateAll hite "error" predFalse
        
        hite = annotateVar $ fixBottom $ removeUnderscore bad_hite
        simp = simplifyReqsFull hite
        
        f x = do putStrLn $ "\n? " ++ show x
                 y <- reduce hite x
                 let yy = simp y
                 putStrLn $ "=\n" ++ prettyReqs y
                 return ("? " ++ show x ++ "\n= \n" ++ prettyReqs y, y)


---------------------------------------------------------------------
-- HITE MANIPULATORS

-- remove all _ calls, replace them with _|_ instead
removeUnderscore :: Hite -> Hite
removeUnderscore x = mapExpr f $ x{funcs = filter ((/= "_") . funcName) (funcs x)}
    where
        f (Call (CallFunc "_") []) = Bottom
        f (CallFunc "_") = Bottom
        f x = x


fixBottom :: Hite -> Hite
fixBottom hite = hite{funcs = map f (funcs hite), datas = newData : datas hite}
    where
        newData = Data "Internal" [Ctor "InternalA" [], Ctor "InternalB" []]
        newBody = Case (Make "InternalA" []) [("InternalB", Call (CallFunc "_") [])]
        
        f (Func "error" args body pos) = Func "error" args newBody pos
        f x = x


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

out :: Depth -> String -> IO ()
out depth msg = putStrLn $ replicate (depth*2) ' ' ++ msg



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



reduce :: Hite -> Reqs -> IO Reqs
reduce hite x = reduceMany hite [] 1 x


reduceOne :: Hite -> [Req] -> Bool -> Depth -> Req -> IO Reqs
reduceOne hite pending supress depth orig_req =
    do
        if supress then return () else out depth (show orig_req)
        case orig_req of
            r | r `elem` pending -> do out depth "True -- Pending tied back"
                                       return predTrue

            (Req (Var a b) _ _) -> 
                if b == "*" || b == "main" then
                    return $ predLit orig_req
                else
                    onwards $ propagate hite orig_req

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do x <- reduceMany hite pending depth within
                                     if on == "main" then
                                         return $ mapPredLit starToMain x
                                      else
                                         reduceMany hite (orig_req:pending) depth $ propagateAll hite on x

            r -> onwards $ backward hite r
    where
        onwards = reduceMany hite p2 depth
            where
                p2 = case orig_req of
                    Req _ _ _ -> orig_req : pending
                    _ -> pending

        starToMain (Req on path opts) = predLit $ Req (mapExpr f on) path opts
        f (Var x "*") = Var x "main"
        f x = x


reduceMany :: Hite -> [Req] -> Depth -> Reqs -> IO Reqs
reduceMany hite pending depth xs | depth > maxCheckDepth = do putStrLn "Lazy, giving up (False)"
                                                              return predFalse

reduceMany hite pending depth orig_xs =
        case simp (reduceManySmall hite orig_xs) of
             PredLit x -> reduceOne hite pending False depth x
             x | null (allPredLit x) -> return x
             xs -> f xs
    where
        f xs =
            do
                let reqs = nub $ allPredLit xs
                out depth ("+ " ++ show orig_xs)
                out depth ("  " ++ show xs)
                res <- g xs reqs
                out depth ("- " ++ show res)
                return res

        simp = simplifyMid hite . simpler

        g reqs [] = return reqs
        g reqs (x:xs) = do out (depth+1) ("+ " ++ show x)
                           res <-
                               if x `elem` allPredLit reqs then
                                   do r <- reduceOne hite pending True (depth+2) x
                                      out (depth+1) ("- " ++ show r)
                                      return $ simp $ mapPredLit (replace x r) reqs
                               else
                                   do out (depth+1) ("- ignored for now")
                                      return reqs
                           g res xs

        replace from to x = if x == from then to else predLit x



simplifyMid hite x = if simplifyRegular then simplifyReqs False hite x else x

