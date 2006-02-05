
module Checker.Backward(backward) where

import Hite
import Constraint
import Maybe
import List
import General.General


backward :: Hite -> Req -> Reqs

backward hite (Req (Var a b) path opts Nothing) = predLit $ Req (Var a b) path opts Nothing

backward hite (Req (Sel a b) path opts Nothing) = predLit $ Req a (pathIntegrate b path) opts Nothing

backward hite (Req (Call (CallFunc name) params) path opts Nothing) =
        if length params == length args then
            predLit $ Req res path opts Nothing
        else
            error $ "Backward: unsaturated " ++ name
    where
        (Func _ args body _) = getFunc name hite
        
        rename = zip args params
        res = blurExpr $ mapExpr f body
        
        f (Var a _) = fromJustNote "backward" $ lookup a rename
        f x = x
        
        

backward hite (Req (Case on alts) path opts Nothing) = predAnd $ map f alts
    where
        others = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
        
        f (ctor, expr) = predOr [
                predLit $ Req on pathLambda (others \\ [ctor]) Nothing,
                predLit $ Req expr path opts Nothing
            ]


backward hite (Req (Make x ys) path opts Nothing) = predAnd $ pre : zipWith f cArgs ys
    where
        cArgs = ctorArgs $ getCtor x hite

        f arg e = predLit $ Req e (pathQuotient arg path) opts Nothing
        
        pre = if pathIsEwp path then
                  predBool (x `elem` opts)
              else
                  predTrue


backward hite (Req orig@(Htap name args alt) path opts Nothing) = predAnd $
    [
        predLit $ Req alt path opts Nothing,
        predLit $ Req (Make name (map f args)) path opts Nothing
    ]
    where
        f Nothing = orig
        f (Just x) = x


backward hite (Req orig@(Repeat expr alt) path opts Nothing) = predAnd $
    [
        predLit $ Req alt path opts Nothing,
        predLit $ Req (unrollExpr orig) path opts Nothing
    ]

        
backward hite a = error $ "Backward: " ++ show a

