
module Checker.CaseCheck(caseCheck) where

import Hite
import General
import List

import Constraint

import Checker.Propagate
import Checker.Backward
import Checker.Fixpoint


caseCheck :: Hite -> IO ()
caseCheck hite = do res <- solveReqs solveProp hite putStrLn (incompleteCases hite)
                    if res then putStrLn "Success!" else putStrLn "Failed"



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



incompleteCases :: Hite -> Reqs -- (FuncName, Expr, [CtorName])]
incompleteCases hite = Ands [PredAtom $
        Req func (Var (varArg on) []) lambda opts  |
        Func func _ body <- funcs hite,
        c@(Case on alts) <- allExpr body,
        opts <- [fsts alts],
        allOpts <- [map ctorName $ ctors $ getDataFromCtor (head opts) hite],
        not $ null $ allOpts \\ opts]

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
