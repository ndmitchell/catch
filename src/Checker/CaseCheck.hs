
module Checker.CaseCheck(caseCheck) where

import Hite
import General
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
caseCheck bad_hite = putStrLn $ f 50 output res 
    where
        (res,output) = runReqMonad [] (solves hite (generate hite))
        hite = annotate bad_hite
        
        f _ [] res = "\nRESULT: " ++ show res
        f 0 _ res = "\nNON TERMINATION"
        f n (x:xs) res = x ++ "\n" ++ f (n-1) xs res




solve :: Hite -> Req -> ReqMonad (Reqs, Output)
solve hite r@(Req on path opts) = 
    case on of
        Var a b | b == "main" -> return (PredAtom r, [])
                | otherwise   -> solves hite $ propagate hite r
        x -> solves hite $ backward hite r
                      


simplify :: Reqs -> ReqMonad Reqs
simplify x = do y <- finds $ reduce x
                return $ reduce y


reduce x = mapReq (PredAtom . simpReq) (reducePred x)


simpReq = blurReq . reduceReq


blurReq (Req a b c) = Req a (blurRegExp b) c


blurRegExp x = f x
    where
        f (RStar x) = RStar (f x)
        f (RUni x) = RUni (map f x)
        f (RCon x) = RCon (g x)
        f x = x
        
        g (a:b:c:d) | a == b && b == c =
            a : b : RStar c :
            (
                if not (null d) && (head d == RStar c) then
                    tail d
                else
                    d
            )
        g (x:xs) = x : g xs
        g [] = []




solves :: Hite -> Reqs -> ReqMonad (Reqs, Output)
solves hite x = do q <- simplify x
                   apply q
    where
        apply q = case reduce q of
            PredAtom y -> do (res, out) <- solve hite y
                             return (res, ("* " ++ show y) : out)

            Ors [] ->  return (PredFalse, [])
            Ands [] -> return (PredTrue, [])

            xs -> f xs
        
    
        f xs = do let reqs = nub $ allReq xs
                  mids <- mapM (solve hite) reqs
                  let res = reduce $ mapReq (g (zip reqs (map fst mids))) xs
                      
                      outF = "> " ++ show xs
                      outM = concatMap h (zip reqs (map snd mids))
                      outL = "< " ++ show res
                      
                  return (res, [outF] ++ outM ++ [outL])
        
        g ren r = fromJust $ lookup r ren
        
        h (from, out) = (indent $ "+ " ++ show from) : (indents $ indents $ out)



indent x = "    " ++ x
indents = map indent



caseCheck2 :: Hite -> IO ()
caseCheck2 bad_hite = do putStrLn $ disp $ full $ reduce $ generate hite
{-                    res <- solveReqs solveProp hite putStrLn (incompleteCases hite)
                    if res then putStrLn "Success!" else putStrLn "Failed"
                    -}
    where
        hite = annotate bad_hite
        
        
        disp :: [Reqs] -> String
        disp x = f 0 x
            where
                f 10 (x:xs) = "\nNON TERMINATION"
                f n  (x:xs) = show n ++ ": " ++ show x ++ "\n" ++ f (n+1) xs
                f n  []     = "\nMAIN: " ++ show allMains

            
                allMains = filter isMain $ concatMap allReq x
                
                isMain (Req (Var a b) _ _) = b == "main"
                isMain _ = False
        
        
        
        full :: Reqs -> [Reqs]
        full reqs | null $ allReq reqs = []
                  | otherwise = reqs : full (next reqs)

        
        next :: Reqs -> Reqs
        next x = reduce $ mapReq (backward hite) $ mapReq (propagate hite) x
        
        
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
generate hite = Ands [PredAtom $
        Req on lambda opts |
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
