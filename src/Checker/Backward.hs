
module Checker.Backward(backward, backwardRepeat, backwardRepeatPred, mcasePred, predNot, backwardRepeatAll) where

import Hite
import Constraint
import Maybe
import List
import General.General


backward :: Hite -> Req -> Reqs

backward hite (Req (Sel a b) path opts) = predLit $ Req a (pathIntegrate b path) opts

backward hite (Req Bottom path opts) = predFalse
backward hite (Req (Call Bottom _) path opts) = predFalse

backward hite (Req (Call (CallFunc name) params) path opts) =
        if length params == length args then
            predLit $ Req res path opts
        else
            error $ "Backward: unsaturated " ++ name
    where
        (Func _ args body _) = getFunc hite name
        
        rename = zip args params
        res = blur $ {- selToPath $ -} mapExpr f body
        
        f (Var a) = fromJustNote "backward" $ lookup a rename
        f x = x
        
        

{-
backward hite (Req (Case on alts) path opts) = predAnd $ map f alts
    where
        others = getCtorsFromCtor hite (fst $ head alts)
        
        f (ctor, expr) = predOr [
                predLit $ Req on pathLambda (others \\ [ctor]),
                predLit $ Req expr path opts
            ]
-}

backward hite (Req (MCase alts) path opts) = predAnd $ map f alts
    where
        f (MCaseAlt cond expr) = predOr [reqsNot hite $ mcasePred hite cond, predLit $ Req expr path opts]



backward hite (Req (Make x ys) path opts) = predAnd $ pre : zipWith f cArgs ys
    where
        cArgs = ctorArgs $ getCtor hite x

        f arg e = if pathIsEmpty q then
                      predTrue
                  else
                      predLit $ Req e q opts
            where q = pathQuotient arg path
            
        
        pre = if pathIsEwp path then
                  predBool (x `elem` opts)
              else
                  predTrue


backward hite (Req orig@(Repeat expr alt) path opts) = predAnd $
    [
        predLit $ Req alt path opts,
        predLit $ Req (unrollExpr orig) path opts
    ]

backward hite (Req (Msg _) path opts) = predFalse


backward hite all@(Req a b c) = error $ "backward, unhandled: " ++ show all ++ ": " ++ case a of
    Call x xs -> "call" ++ show (output x, map output xs)
    CallFunc x -> "callfunc"
    Make x xs -> "make"
    Repeat _ _ -> "repeat"
    MCase _ -> "mcase"
    _ -> "other"

backward hite a = error $ "Backward: " ++ show a


-- small reduce - only reduce as far as call's
-- or var's or repeat's
-- will take a bounded (hopefully small) amount of time

backwardRepeat :: Hite -> Req -> Reqs
backwardRepeat hite x = case x of
        (Req (Var a) _ _ ) -> predLit x
        (Req (Repeat _ _) _ _) -> predLit x
        (Req (Call _ _) _ _) -> predLit x
        x -> backwardRepeatPred hite (backward hite x)


backwardRepeatAll :: Hite -> ReqAlls -> ReqAlls
backwardRepeatAll hite x = mapPredLit f x
    where
        f (ReqAll on within) = predLit $ ReqAll on (backwardRepeatPred hite within)

backwardRepeatPred :: Hite -> Reqs -> Reqs
backwardRepeatPred hite x = mapPredLit (backwardRepeat hite) x





-- mcasePred :: MCaseAlt -> Pred
-- What has to be true for this condition to be picked
mcasePred :: Hite -> MCasePred -> Reqs
mcasePred hite cond = f cond
    where
        f (MCaseAnd xs) = predAnd $ map f xs
        f (MCaseOr  xs) = predOr  $ map f xs
        f (MCaseLit expr cond) = backwardRepeat hite $ Req expr pathLambda [cond]

