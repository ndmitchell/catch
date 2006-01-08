
module Checker.Backward(backward) where

import Hite
import Constraint
import Maybe
import List
import General


backward :: Hite -> Req -> Reqs

backward hite (Req (Var a b) path opts) = predLit $ Req (Var a b) path opts

backward hite (Req (Sel a b) path opts) = predLit $ Req a (regConcat [regLit b, path]) opts


backward hite (Req (Call (CallFunc name) params) path opts) =
        if length params == length args then
            predLit $ Req res path opts
        else
            error $ "Backward: unsaturated " ++ name
    where
        (Func _ args body _) = getFunc name hite
        
        rename = zip args params
        res = mapExpr f body
        
        f (Var a _) = fromJustNote "backward" $ lookup a rename
        f x = x


backward hite (Req (Case on alts) path opts) = predAnd $ map f alts
    where
        others = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
        
        f (ctor, expr) = predOr [
                predLit $ Req on regLambda (others \\ [ctor]),
                predLit $ Req expr path opts
            ]


backward hite (Req (Make x ys) path opts) = predAnd $ pre : zipWith f cArgs ys
    where
        cArgs = ctorArgs $ getCtor x hite

        f arg e = predLit $ Req e (quotient arg path) opts
        
        pre = if isEwp path then
                  predBool (x `elem` opts)
              else
                  predTrue
        
        

backward hite a = error $ "Backward: " ++ show a



-- from the semantics in the paper
-- backwardStep :: Hite -> Req -> Pred Req



 

{-



backweird :: Hite -> FuncName -> CallStack -> Exp -> Req -> (CallStack, Pred)
-- hite (parentname and condition) exp (condition for exp) -> (recursive calls, precondition)
backweird hite func calls expr req@(Req _ _ path ctors) =
        --if length calls == length (fst res) then res
        --else error $ show $ (length calls, length (fst res), expr)
        --if length calls > 10 then error (show calls) else 
        --debugmsg ("backward", length calls) $
        
        --debugmsg ("[enter backweird on " ++ func ++ " with " ++ show req ++ "]") (debugmsg smsg res)
        --debugmsg ("backward",expr) $
        
        res
    where
        smsg = reverse (reverse msg)
        msg = "[done backweird on " ++ func ++ ", with " ++ show req ++ " gives " ++ show (snd res) ++ "]"
    
        res = case expr of
            Var varId varPath -> (,) calls $ PredReq (Req func varId (varPath `pathJoin` path) ctors)

            Make ctor args ->
                    if isPathEwp path && not (ctor `elem` ctors)
                    then (calls, PredFalse "")
                    else f calls [] (zip args [1..])
                where
                    f c r [] = (c, Ands r)
                    f c r ((expr, n):rest) =
                        case pathMove path (Sel ctor n) of
                           Nothing -> f c r rest
                           Just x -> f c2 (r2:r) rest
                                where (c2, r2) = backweird hite func c expr req{reqPath=x}
            
            Call (CallFunc name) args ->
                    case addStack [] calls of
                        Just x -> (x, PredTrue)
                        Nothing -> mapReqState f calls2 pred3
                where
                    -- try adding it to the call stack, if you succeed return the new call stack
                    addStack done [] = Nothing
                    addStack done (x@(Req parentName _ parentPath parentCtors, recs):xs)
                        | name == parentName && parentPath == path && parentCtors == ctors =
                            Just $ (reverse done) ++ (fst x, expr:recs) : xs
                        | otherwise = addStack (x:done) xs
                    
                    
                    (((_,recs):calls2), pred2) =
                        backweird hite name ((reqMe, []):calls) (getFunc hite name) reqMe
                    reqMe = req{reqFunc=name}
                        
                    pred3 = closure hite recs (simpPredReq pred2)
                
                    f state req =
                        if length args <= reqVar req -1
                        then error $ show $ (name, expr, args, reqVar req - 1)
                        else backweird hite func state (args !! (reqVar req - 1)) req

            Case test opts -> 
                    f calls [] opts
                where
                    f state done [] = (state, Ands done)
                    f state done ((ctor,code):rest) = f s3 (Ors [p1,p2]:done) rest
                        where
                            (s2, p1) = backweird hite func state test req2
                            (s3, p2) = backweird hite func s2    code req

                            req2 = notReq hite $ req{reqPath=lambda, reqCtors=[ctor]}

-}