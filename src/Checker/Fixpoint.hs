
module Checker.Fixpoint(fixpoint) where

import Hite
import Constraint


{-
data Close = CFwd [Sel]
           | CBwd CtorName [Exp] -- all exp must be var
           | CErr {cerr :: String}
           | CId
           deriving (Eq, Show)

isCErr (CErr _) = True; isCErr _ = False
isCFwd (CFwd _) = True; isCFwd _ = False
isCId x = x == CId

cfwdSel (CFwd x) = x
-}

-- \Delta, closure
fixpoint :: Hite -> Req -> [Expr] -> Either Reqs String
fixpoint hite p0 [] = Left $ PredAtom p0
fixpoint hite p0 _  = Right "todo"


{-
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
-}
