
module Checker.Backward(backward) where

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
        (Func _ args body _) = getFunc name hite
        
        rename = zip args params
        res = blurExpr $ {- selToPath $ -} mapExpr f body
        
        f (Var a _) = fromJustNote "backward" $ lookup a rename
        f x = x
        
        

backward hite (Req (Case on alts) path opts) = predAnd $ map f alts
    where
        others = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
        
        f (ctor, expr) = predOr [
                predLit $ Req on pathLambda (others \\ [ctor]),
                predLit $ Req expr path opts
            ]


backward hite (Req (Make x ys) path opts) = predAnd $ pre : zipWith f cArgs ys
    where
        cArgs = ctorArgs $ getCtor x hite

        f arg e = predLit $ Req e (pathQuotient arg path) opts
        
        pre = if pathIsEwp path then
                  predBool (x `elem` opts)
              else
                  predTrue


backward hite (Req orig@(Repeat expr alt) path opts) = predAnd $
    [
        predLit $ Req alt path opts,
        predLit $ Req (unrollExpr orig) path opts
    ]



backward hite all@(Req a b c) = error $ show all ++ ": " ++ case a of
    Call x xs -> "call" ++ show (x,xs)
    CallFunc x -> "callfunc"
    Make x xs -> "make"
    _ -> "other"

backward hite a = error $ "Backward: " ++ show a
