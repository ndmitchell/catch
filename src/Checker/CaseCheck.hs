
module Checker.CaseCheck(caseCheck) where

import Hite
import General.General
import List
import Maybe

import Checker.Propagate
import Checker.Backward

import Constraint
import Options


import Debug.Trace

traceThis x = x -- trace ("{O{" ++ show x ++ "}C}") x


type Output = [String]



caseCheck :: Hite -> IO ()
caseCheck bad_hite = putStrLn $ f 0 output res 
    where
        (res,output) = solves hite [] (generateEnv hite $ generate hite)
        hite = annotate $ normalise bad_hite
        
        f n [] res = "\nRESULT(" ++ show n ++ "):\n" ++ prettyReqs (simplifyReqsFull hite res)
        f n (x:xs) res | n > maxCompute = "\nNON TERMINATION"
                       | otherwise = x ++ "\n" ++ f (n+1) xs res



simpler x = mapPredLit (predLit . blurReqPath) (reducePred x)



type State = (
                Reqs, -- the current value
                Output -- the output to give
             )

addOut :: String -> State -> State
addOut msg (r, o) = (r, msg:o)


solve :: Hite -> [Req] -> Req -> State
solve hite pending r | r `elem` pending = (predTrue, [])

solve hite pending r@(Req _ path opts Nothing) | pathIsEmpty path = (predTrue, [])

solve hite pending r@(Req (CallFunc "_") path opts Nothing) = (predFalse, [])

solve hite pending r@(Req (Var a b) path opts Nothing) 
    | b == "main" = (predLit r, [])
    | otherwise   = solvePending hite r pending $ generateEnv hite (predAnd [predLit r])

solve hite pending r@(Req (Call (Var a b) params) path opts Nothing) =
    error $ "solve, todo: " ++ show r

solve hite pending r = solvePending hite r pending $ backward hite r


solvePending :: Hite -> Req -> [Req] -> Reqs -> State
solvePending hite p pending x -- addOut "here" s
        | predLit p == x = error $ "Circular reasoning, " ++ show p
        | otherwise = s
        -- | p `elem` allPredLit rs = addOut ("@ " ++ show (p, rs, rs2)) (rs2, os)
        -- | otherwise = s
    where
        f r | r == p = predTrue
        f x = predLit x
        
        s@(rs, os) = solves hite (p:pending) x
        rs2 = mapPredLit f rs


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



generate :: Hite -> Reqs
generate hite = predAnd [predLit $
        Req on pathLambda opts Nothing |
        c@(Case on alts) <- allExpr hite,
        opts <- [fsts alts],
        allOpts <- [map ctorName $ ctors $ getDataFromCtor (head opts) hite],
        not $ null $ allOpts \\ opts]


generateEnv :: Hite -> Reqs -> Reqs
generateEnv hite xs = res
    where
        res = predAnd [predLit $ Req on path opts (Just (f name args)) |
                    PredLit (Req on path opts _) <- fromAnd xs,
                    Func name args _ _ <- funcs hite]
        
        f name args = Call (CallFunc name) (map (`Var` name) args) 



-- and annotations as to which function each variable is in
annotate :: Hite -> Hite
annotate h = mapFunc f h
    where
        f (Func name args body k) = Func name args (mapExpr (g name) body) k
        g name (Var x y) = Var x name
        g name x = x

