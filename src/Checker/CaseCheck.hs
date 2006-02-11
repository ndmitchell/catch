
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
caseCheck bad_hite = putStrLn $ f 0 output res 
    where
        (res,output) = solves hite [] (generate hite)
        hite = annotateVar $ removeUnderscore bad_hite
        
        f n [] res = "\nRESULT(" ++ show n ++ "):\n" ++ prettyReqs (simplifyReqsFull hite res)
        f n (x:xs) res | n > maxCompute = "\nNON TERMINATION"
                       | otherwise = x ++ "\n" ++ f (n+1) xs res


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


solve :: Hite -> [Req] -> Req -> State
solve hite pending r | r `elem` pending = (predTrue, [])

solve hite pending r@(Req _ path opts) | pathIsEmpty path = (predTrue, [])

solve hite pending r@(Req (Var a b) path opts) 
    | b == "main" = (predLit r, [])
    | b == "*"    = (predLit r, [])
    | otherwise   = solvePending hite r pending $ propagate hite r

solve hite pending (ReqAll on within) = (r2, o2 ++ o)
        -- ( {- if allStar then propagateAll destar else -} propagate hite destar, o)
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
    
    {-
        pl = predLit p
        
        f rs = case rs of 
            PredAnd xs -> g $ predAnd $ filter (/= pl) xs
            PredOr  xs -> g $ predOr  $ filter (/= pl) xs
        
        
        -- g x = trace (show (p,x)) x
        
        g x | False = - p `elem` allPredLit x = - error $ show ("solvePending",p, rs)
            | otherwise = trace ("PENDING: " ++ show p ++ "\nORIGINAL: " ++ show rs ++ "\nFIXED: " ++ show x) (x, os)
      -}


simplifyMid hite x = if simplifyRegular then simplifyReqs False hite x else x


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


---------------------------------------------------------------------
-- GENERATION FUNCTIONS

generate :: Hite -> Reqs
generate = if propagateSimp then generateSimp else generateComplex


generateComplex :: Hite -> Reqs
generateComplex hite = predAnd $ concatMap (\(Func name _ body _) -> f name predFalse (mapExpr str body)) $ funcs hite
    where
        f :: FuncName -> Reqs -> Expr -> [Reqs]
        f name hist (Case on alts) = newItem ++ concatMap g alts
            where
                allCtor = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
                
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


generateSimp :: Hite -> Reqs
generateSimp hite = predAnd [predLit $
        Req on pathLambda opts |
        c@(Case on alts) <- allExpr hite,
        opts <- [fsts alts],
        allOpts <- [map ctorName $ ctors $ getDataFromCtor (head opts) hite],
        not $ null $ allOpts \\ opts]


