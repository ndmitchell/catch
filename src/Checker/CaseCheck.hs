
module Checker.CaseCheck(caseCheck) where

import Hite
import General.General
import List
import Maybe

import Checker.Propagate
import Checker.Backward

import Constraint
import Constraint.ReqMonad

{-

import Checker.Propagate
import Checker.Fixpoint
-}

type Output = [String]



caseCheck :: Hite -> IO ()
caseCheck bad_hite = putStrLn $ f 10000 output res 
    where
        (res,output) = solves hite [] (generate hite)
        hite = annotate bad_hite
        
        f _ [] res = "\nRESULT: " ++ show (reduceReqs res)
        f 0 _ res = "\nNON TERMINATION"
        f n (x:xs) res = x ++ "\n" ++ f (n-1) xs res



reduceReqs x = simplifyPred [RuleAnd ruleAnd] x
    where
        ruleAnd (Req a1 b1 c1) (Req a2 b2 c2)
            | a1 == a2 && c1 == c2 =
            Just (Req a1 (regUnion [b1,b2]) c1)
        ruleAnd _ _ = Nothing



simpler x = mapPredLit (predLit . simpReq) (reducePred x)


simpReq = blurReq . reduceReq


blurReq (Req a b c) = Req a (blur b) c



type State = (
                Reqs, -- the current value
                Output -- the output to give
             )

addOut :: String -> State -> State
addOut msg (r, o) = (r, msg:o)


solve :: Hite -> [Req] -> Req -> State
solve hite pending r | r `elem` pending = (predTrue, [])

solve hite pending r@(Req _ path opts) | isEmpty path = (predTrue, [])

solve hite pending r@(Req (CallFunc "_") path opts) = (predFalse, [])

solve hite pending r@(Req (Var a b) path opts) 
    | b == "main" = (predLit r, [])
    | otherwise   = solves hite (r:pending) $ propagate hite r

solve hite pending r = solves hite (r:pending) $ backward hite r



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
                res = simpler $ mapPredLit (g rens) xs
            
                outF = "> " ++ show xs
                outM = concatMap h (zip reqs mids)
                outL = "< " ++ show res


        g ren r = case lookup r ren of
                      Just x -> x
                      Nothing -> predLit r -- has already been replaced once
                              -- sometimes mapPredLit traverses twice
        
        h (from, (final, out)) =
            (indent $ "+ " ++ show from) :
            (indents $ indents $ out) ++
            [indent $ indent $ show final]



indent x = "  " ++ x
indents = map indent


{-
caseCheck2 :: Hite -> IO ()
caseCheck2 bad_hite = do putStrLn $ disp $ full $ simpler $ generate hite
--                    res <- solveReqs solveProp hite putStrLn (incompleteCases hite)
--                    if res then putStrLn "Success!" else putStrLn "Failed"
                    
    where
        hite = annotate bad_hite
        
        
        disp :: [Reqs] -> String
        disp x = f 0 x
            where
                f 10 (x:xs) = "\nNON TERMINATION"
                f n  (x:xs) = show n ++ ": " ++ show x ++ "\n" ++ f (n+1) xs
                f n  []     = "\nMAIN: " ++ show allMains

            
                allMains = filter isMain $ concatMap allPredLit x
                
                isMain (Req (Var a b) _ _) = b == "main"
                isMain _ = False
        
        
        
        full :: Reqs -> [Reqs]
        full reqs | null $ allReq reqs = []
                  | otherwise = reqs : full (next reqs)

        
        next :: Reqs -> Reqs
        next x = simpler $ mapReq (backward hite) $ mapReq (propagate hite) x
        -}
        
{-

solveReqs :: (Hite -> (String -> IO ()) -> Req -> IO Bool) ->
              Hite -> (String -> IO ()) -> Reqs -> IO Bool
solveReqs with hite out c = do out $ "Solve: " ++ show c
                               f c
    where
        out2 x = out $ "  " ++ x
    
        f PredTrue  = return True
        f PredFalse = return False
        f (PredAtom x) = with hite out2 x
        f (Ors  x) = g or  x
        f (Ands x) = g and x
        
        g comb xs = do res <- mapM f xs
                       return $ comb res



solveProp :: Hite -> (String -> IO ()) -> Req -> IO Bool
solveProp hite out c = do out $ "Start: " ++ show c
                          let (lefts, rights) = splitEither $ propagate hite c
                          mapM_ (\x -> out $ "ERROR: propagate: " ++ x) rights
                          solveReqs solveBack hite out (Ands $ map PredAtom lefts)


solveBack :: Hite -> (String -> IO ()) -> Req -> IO Bool
solveBack hite out c = do out $ "Backwards: " ++ show c
                          case backward hite c of
                              Right x -> do out $ "ERROR: backward: " ++ x
                                            return False
                              Left (a, b) ->
                                  do out $ "Result: " ++ show a
                                     out $ "Calls: " ++ show b
                                     case fixpoint hite a b of
                                         Right x -> do out $ "ERROR: fixpoint: " ++ x
                                                       return False
                                         Left  x -> do out $ "Fixpoint: " ++ show x
                                                       solveReqs solveProp hite out x
-}
{-                         



solveCase :: Hite -> (String -> IO ()) -> Req -> IO ()
solveCase hite out c = do out $ "Initial: " ++ show c
                          case backward hite c of
                              Right x -> out $ "ERROR: backward: " ++ x
                              Left (a,b) -> case fixPoint hite a b of
                                    Right x -> out $ "ERROR: fixPoint: " ++ x
                                    Left x = 
                            
                          let (lefts, rights) = splitEither $ propagate hite c
    where
        f x = do out $ "Propagate: " ++ show x
                 solveCase hite (\a -> out $ "  " ++ a) x
-}



generate :: Hite -> Reqs
generate hite = predAnd [predLit $
        Req on regLambda opts |
        c@(Case on alts) <- allExpr hite,
        opts <- [fsts alts],
        allOpts <- [map ctorName $ ctors $ getDataFromCtor (head opts) hite],
        not $ null $ allOpts \\ opts]



-- and annotations as to which function each variable is in
annotate :: Hite -> Hite
annotate h = mapFunc f h
    where
        f (Func name args body k) = Func name args (mapExpr (g name) body) k
        g name (Var x y) = Var x name
        g name x = x


{-




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

-}
