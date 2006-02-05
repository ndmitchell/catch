
module Checker.Backward(backward) where

import Hite
import Constraint
import Maybe
import List
import General.General


backward :: Hite -> Req -> Reqs

-- backward hite (Req (Var a b) path opts Nothing) = predLit $ Req (Var a b) path opts Nothing

backward hite (Req (Sel a b) path opts x) = predLit $ Req a (pathIntegrate b path) opts x

backward hite (Req func@(Call (CallFunc name) params) path opts Nothing) =
        predLit $ Req (expandFunction hite func) path opts Nothing
        

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

backward hite (Req orig@(Repeat expr alt) path opts Nothing) = predAnd $
    [
        predLit $ Req alt path opts Nothing,
        predLit $ Req (unrollExpr orig) path opts Nothing
    ]


backward hite (Req (Var arg func) path opts (Just call@(Call (CallFunc x) xs)))  = 
        if x == func then
            predLit $ Req (xs !! (pos - 1)) path opts Nothing
        else
            predLit $ Req (Var arg func) path opts (Just $ expandFunction hite call)
    where
        pos = getArgPos func arg hite

    
    
backward hite (Req (Var arg func) path opts (Just (Case on alts))) = predAnd $ map f alts
    where
        others = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
        
        f (ctor, expr) = predOr [
                predLit $ Req on pathLambda (others \\ [ctor]) Nothing,
                predLit $ Req (Var arg func) path opts (Just expr)
            ]

backward hite (Req (Var arg func) path opts (Just (Make x ys))) = predAnd $ map f ys
    where
        f x = predLit $ Req (Var arg func) path opts (Just x)

backward hite (Req (Var arg func) path opts (Just (Sel x _))) = backward hite (Req (Var arg func) path opts (Just x))

backward hite (Req (Var arg func) path opts (Just (Var _ _))) = predTrue

backward hite (Req (Var arg func) path opts (Just call@(Call (Var _ _) _)))  = predTrue

backward hite x = error $ "Backward: " ++ show x



expandFunction :: Hite -> Expr -> Expr
expandFunction hite (Call (CallFunc name) params) =
        if length params == length args then
            res
        else
            error $ "Backward: unsaturated " ++ name
    where
        (Func _ args body _) = getFunc name hite
        
        rename = zip args params
        res = blurExpr $ mapExpr f body
        
        f (Var a _) = fromJustNote "backward" $ lookup a rename
        f x = x
