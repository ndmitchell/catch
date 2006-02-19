
module Checker.CaseCheck(caseCheck) where

import Hite
import General.General
import List
import Maybe

import Checker.Propagate
import Checker.Backward

import Constraint
import Options


type Output = [String]



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
        reqs = generate hite
        hite = annotateVar $ removeUnderscore bad_hite
        simp = simplifyReqsFull hite
        
        f x = do putStrLn $ "\n? " ++ show x
                 y <- reduce hite x
                 let yy = simp y
                 putStrLn $ "=\n" ++ prettyReqs y
                 return ("? " ++ show x ++ "\n= \n" ++ prettyReqs y, y)

{-
        (res,output) = solves hite [] (generate hite)
        
        f n [] res = 
        f n (x:xs) res | n > maxCompute = "\nNON TERMINATION"
                       | otherwise = x ++ "\n" ++ f (n+1) xs res
-}

---------------------------------------------------------------------
-- HITE MANIPULATORS

-- remove all _ calls, replace them with _|_ instead
removeUnderscore :: Hite -> Hite
removeUnderscore x = mapExpr f $ x{funcs = filter ((/= "_") . funcName) (funcs x)}
    where
        f (Call (CallFunc "_") []) = Bottom
        f (CallFunc "_") = Bottom
        f x = x


-- and annotations as to which function each variable is in
annotateVar :: Hite -> Hite
annotateVar h = mapFunc f h
    where
        f (Func name args body k) = Func name args (mapExpr (g name) body) k
        g name (Var x y) = Var x name
        g name x = x


---------------------------------------------------------------------
-- UTILITY FUNCTIONS


simpler x = blurReqsPath (reducePred x)


type State = (
                Reqs, -- the current value
                Output -- the output to give
             )

addOut :: String -> State -> State
addOut msg (r, o) = (r, msg:o)


---------------------------------------------------------------------
-- CORE FUNCTIONS


data Trace = Trace String | Indent | Outdent


type Depth = Int

output :: Depth -> String -> IO ()
output depth msg = putStrLn $ replicate (depth*2) ' ' ++ msg



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
reduceOne hite pending supress depth x =
    do
        if supress then return () else output depth (show x)
        case x of
            r | r `elem` pending -> do output depth "True -- Pending tied back"
                                       return predTrue

            (Req (Var a b) _ _) -> 
                if b == "*" || b == "main" then
                    return $ predLit x
                else
                    onwards $ propagate hite x

            (Req _ path opts) | pathIsEmpty path -> return predTrue
    
            
            (ReqAll on within) -> do x <- reduceMany hite pending depth within
                                     if on == "main" then
                                         return $ mapPredLit starToMain x
                                      else
                                         reduceMany hite (allPredLit x ++ pending) depth $ propagateAll hite on x

            r -> onwards $ backward hite r
    where
        onwards = reduceMany hite p2 depth
            where
                p2 = case x of
                    Req _ _ _ -> x : pending
                    _ -> pending

        starToMain (Req on path opts) = predLit $ Req (mapExpr f on) path opts
        f (Var x "*") = Var x "main"
        f x = x


reduceMany :: Hite -> [Req] -> Depth -> Reqs -> IO Reqs
reduceMany hite pending depth xs | depth > maxCheckDepth = do putStrLn "Lazy, giving up (False)"
                                                              return predFalse

reduceMany hite pending depth xs =
        case simpler (reduceManySmall hite xs) of
             PredLit x -> reduceOne hite pending False depth x
             x | null (allPredLit x) -> return x
             xs -> f xs
    where
        f xs =
            do
                let reqs = nub $ allPredLit xs
                output depth ("+ " ++ show xs)
                reqs2 <- mapM g reqs
                let ren = zip reqs reqs2
                    res = simplifyMid hite $ simpler $ mapPredLit (rename ren) xs
                output depth ("- " ++ show res)
                return res

        g x = do output (depth+1) ("+ " ++ show x)
                 res <- reduceOne hite pending True (depth+2) x
                 output (depth+1) ("- " ++ show res)
                 return res


        rename ren r = case lookup r ren of
                          Just x -> x
                          Nothing -> predLit r -- has already been replaced once
                                  -- sometimes mapPredLit traverses twice

simplifyMid hite x = if simplifyRegular then simplifyReqs False hite x else x


{-

-- reduce and reduces
-- both change so resulting predicate is only on Var main and *

-- perform all required
reduce :: Hite -> [Req] -> Depth -> Req -> IO Reqs
reduce hite pending depth x = case x of
    
    (Req _ path opts) | pathIsEmpty path -> predTrue
    
    r | r `elem` pending -> do output depth "* True -- Pending tied back"
                               return predTrue
    
    r -> do 
    
    let (a,b) = solveReqsToVar hite (r:pending) (backward hite r) in (a, b)



-- perform exactly one reduction step
reduceOne :: Hite -> Req -> OutputMonad Reqs
solveReqToVar :: Hite -> [Req] -> Req -> (Reqs, [Trace])
solveReqToVar hite pending x = case x of
    (Req (Var a b) _ _) -> (predLit x, [])
    (ReqAll on within) -> let (a,b) = solveReqsToVar hite pending within in (predLit $ ReqAll on a, b)
    (Req _ path opts) | pathIsEmpty path -> (predTrue, [])
    r | r `elem` pending -> (predTrue, [])
    r -> let (a,b) = solveReqsToVar hite (r:pending) (backward hite r) in (a, b)








-- take a bit Reqs, and a function for solving a single Req
-- and piece it all together
solveReqsWith :: Hite -> (Req -> (Reqs, [Trace])) -> Reqs -> (Reqs, [Trace])
solveReqsWith hite func xs =
        case simpler xs of
             PredLit x -> let (a,b) = func x in (a, Trace ("* " ++ show x) : b)
             x | null (allPredLit x) -> (x, [])
             xs -> f xs
    where
        f xs = (res, outF ++ outM ++ outL)
            where
                reqs = nub $ allPredLit xs
                mids = map func reqs
                rens = zip reqs (map fst mids)
                res1 = simpler $ mapPredLit (g rens) xs
                res = simplifyMid hite res1
            
                outF = [Trace ("+ " ++ show xs), Indent]
                outM = concatMap h (zip reqs mids)
                outL = [Outdent, Trace ("- " ++ show res)]  -- ++ " was (" ++ show res1 ++ ")"


        g ren r = case lookup r ren of
                      Just x -> x
                      Nothing -> predLit r -- has already been replaced once
                              -- sometimes mapPredLit traverses twice
        
        h (from, (final, out)) =
            [Trace ("+ " ++ show from), Indent] ++ out ++ [Outdent, Trace ("- " ++ show final)]




-- take a Reqs on anything, replace it with one whose only literals
-- are Var's, and possible ReqAll's
solveReqsToVar :: Hite -> [Req] -> Reqs -> (Reqs, [Trace])
solveReqsToVar hite pending xs = solveReqsWith hite (solveReqToVar hite pending) xs

solveReqToVar :: Hite -> [Req] -> Req -> (Reqs, [Trace])
solveReqToVar hite pending x = case x of
    (Req (Var a b) _ _) -> (predLit x, [])
    (ReqAll on within) -> let (a,b) = solveReqsToVar hite pending within in (predLit $ ReqAll on a, b)
    (Req _ path opts) | pathIsEmpty path -> (predTrue, [])
    r | r `elem` pending -> (predTrue, [])
    r -> let (a,b) = solveReqsToVar hite (r:pending) (backward hite r) in (a, b)
    
    
        

solveReqOnVar :: Hite -> [Req] -> Reqs
solveReqOnVar = undefined




solve :: Hite -> [Req] -> Req -> State
solve hite pending r | r `elem` pending = (predTrue, [])

solve hite pending r@(Req _ path opts) | pathIsEmpty path = (predTrue, [])

solve hite pending r@(Req (Var a b) path opts) 
    | b == "main" = (predLit r, [])
    | b == "*"    = (predLit r, [])
    | otherwise   = solvePending hite r pending $ propagate hite r

solve hite pending (ReqAll on within) = (r2, o2 ++ o)
        -- ( - if allStar then propagateAll destar else - propagate hite destar, o)
    where
        (r2,o2) = solves hite pending destar
    
        (r,o) = solves hite pending within
        allStar = all f (allPredLit r)
        
        f (Req (Var _ "*") _ _) = True
        f _ = False
        
        destar = mapPredLit (predLit . g) r
        
        g (Req (Var x "*") y z) = Req (Var x on) y z
        g x = x
    

solve hite pending r = solvePending hite r pending $ backward hite r


solvePending :: Hite -> Req -> [Req] -> Reqs -> State
solvePending hite p pending x
        | predLit p == x = addOut (error $ "Circular reasoning, " ++ show p) s
       --  | p `elem` allPredLit rs = addOut ("@ " ++ show (p, rs, rs2)) (rs2, os)
        | otherwise = s
    where
        f r | r == p = predTrue
        f x = predLit x
        
        s@(rs, os) = solves hite (p:pending) x
        rs2 = mapPredLit f rs
    -}
    {-
        pl = predLit p
        
        f rs = case rs of 
            PredAnd xs -> g $ predAnd $ filter (/= pl) xs
            PredOr  xs -> g $ predOr  $ filter (/= pl) xs
        
        
        -- g x = trace (show (p,x)) x
        
        g x | False = - p `elem` allPredLit x = - error $ show ("solvePending",p, rs)
            | otherwise = trace ("PENDING: " ++ show p ++ "\nORIGINAL: " ++ show rs ++ "\nFIXED: " ++ show x) (x, os)
      -}

{-


solves :: Hite -> [Req] -> Reqs -> State
solves hite pending x = 
        case simpler x of
             PredLit y -> addOut ("* " ++ show y) $ solve hite pending y
             x | null (allPredLit x) -> (x, [])
             xs -> f xs
    where
        f xs = (res, [outF] ++ outM ++ [outL])
            where
                reqs = nub $ allPredLit xs
                mids = map (solve hite pending) reqs
                rens = zip reqs (map fst mids)
                res1 = simpler $ mapPredLit (g rens) xs
                res = simplifyMid hite $ res1
            
                outF = "> " ++ show xs
                outM = concatMap h (zip reqs mids)
                outL = "= " ++ show res ++ " was (" ++ show res1 ++ ")"


        g ren r = case lookup r ren of
                      Just x -> x
                      Nothing -> predLit r -- has already been replaced once
                              -- sometimes mapPredLit traverses twice
        
        h (from, (final, out)) =
            (indent $ "+ " ++ show from) :
            (indents $ indents $ out) ++
            [indent $ indent $ show final]
-}


---------------------------------------------------------------------
-- GENERATION FUNCTIONS

generate :: Hite -> [Reqs]
generate = if propagateSimp then generateSimp else generateComplex


userFuncs :: Hite -> [Func]
userFuncs hite = filter f $ funcs hite
    where f x = not $ "%ap" `isPrefixOf` funcName x

generateComplex :: Hite -> [Reqs]
generateComplex hite = concatMap (\(Func name _ body _) -> f name predFalse (mapExpr str body)) $ userFuncs hite
    where
        f :: FuncName -> Reqs -> Expr -> [Reqs]
        f name hist (Case on alts) = newItem ++ concatMap g alts
            where
                allCtor = getCtorsFromCtor hite (fst $ head alts)
                
                newItem = if map fst alts `setEq` allCtor then [] else
                          [predLit $ ReqAll name $ predOr [hist, predLit $ Req on pathLambda (map fst alts)]]
                
                g (typ, expr) = f name (predOr [hist, predLit $ Req on pathLambda (allCtor \\ [typ])]) expr
                
        
        f name hist x = concatMap (f name hist) $ case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Sel x _ -> [x]
            _ -> []

        str (Var x y) = Var x "*"
        str x = x


generateSimp :: Hite -> [Reqs]
generateSimp hite = [predLit $
        Req on pathLambda opts |
        c@(Case on alts) <- allExpr hite,
        opts <- [fsts alts],
        allOpts <- [getCtorsFromCtor hite (head opts)],
        not $ null $ allOpts \\ opts]


