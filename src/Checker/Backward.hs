
module Checker.Backward(backward) where

import Hite
import Constraint


backward :: Hite -> Req -> Either (Req, [Expr]) String
backward hite c = Right "todo"


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