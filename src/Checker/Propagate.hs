

module Checker.Propagate(propagate) where

import Hite
import Checker.Backward
import Constraint
import List
import Options

import Maybe
import General.General


{-
findCallsHistory :: Hite -> (Expr -> Maybe Reqs) -> [(FuncName, Reqs)]
findCallsHistory hite check = concatMap ff $ funcs hite
    where
        ff :: Func -> [(FuncName, Reqs)]
        ff func = map ((,) (funcName func)) (f predFalse (body func))
    
        f :: Reqs -> Expr -> [Reqs]
        f hist x = newItem ++ g x
        
        
            where
                newItem = case check x of
                              Nothing -> []
                              Just y -> [predOr [y, hist]]
            
                g (Case on alts) = concatMap h alts
                    where
                        allCtor = getCtorsFromCtor hite (fst $ head alts)
                        h (typ, expr) = f (predOr [predLit $ Req on pathLambda (allCtor \\ [typ]), hist]) expr

                g x = concatMap (f hist) $ case x of
                        Call x xs -> x : xs
                        Make _ xs -> xs
                        Sel x _ -> [x]
                        _ -> []




propagate :: Hite -> Req -> Reqs
propagate a b | propagateSimp = propagateSimple a b

propagate hite r@(Req (Var arg name) path set) = 
        predAnd $ concatMap (f predFalse . body) $ funcs $ callOne hite
    where
        pos = getArgPos name arg hite
        
        
        f :: Reqs -> Expr -> [Reqs]
        f hist c@(Call (CallFunc n) args) | n == name =
                concatMap (f hist) args ++
                case callArg c pos of
                    Nothing -> error $ "unsaturated: " ++ show (length args, output c) -- [predOr [hist, predFalse]] -- unsaturued use of partial app
                    Just x -> [predOr [hist, predLit $ Req x path set]]
                    
        f hist (Case on alts) = concatMap g alts
            where
                allCtor = getCtorsFromCtor hite (fst $ head alts)
                g (typ, expr) = f (predOr [hist, predLit $ Req on pathLambda (allCtor \\ [typ])]) expr
                
        
        f hist x = concatMap (f hist) $ case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Sel x _ -> [x]
            _ -> []



propagate hite req = error $ "Internal error, complex pattern as subject of req: " ++ show req


propagateAll :: Hite -> FuncName -> Reqs -> Reqs
propagateAll hite on reqs =
        if isTrue reqs
        then predTrue 
        else predAnd $ map reAll $ findCallsHistory hite f
    where
        f c@(Call (CallFunc n) args) | n == on = Just $ mapPredLit g reqs
            where
                g (Req (Var n "*") path opts) = predLit $ Req (fromJust $ callArg c pos) path opts
                    where pos = getArgPos on n hite
                g x = predLit x
        f _ = Nothing

        reAll (name, x) = predLit $ ReqAll name (mapPredLit f x)
            where
                f (Req a b c) = predLit $ Req (mapExpr g a) b c

                g (Var x y) = Var x "*"
                g x = x





-- make all CallFunc's be in the LHS or a Call
callOne :: Hite -> Hite
callOne hite = mapExpr f hite
    where
        f (CallFunc x) = Call (CallFunc x) []
        f (Call (Call (CallFunc x) []) ys) = Call (CallFunc x) ys
        f x = x



propagateSimple :: Hite -> Req -> Reqs
propagateSimple hite (Req (Var arg name) path set) = 
        predAnd $ concatMap f $ allExpr $ callOne hite
    where
        pos = getArgPos name arg hite

        f c@(Call (CallFunc n) _) | n == name = 
                case callArg c pos of
                    Nothing -> [predFalse ] { - $ "Unsaturated use of partial function, " ++ name ++
                                           " (wanted " ++ show pos ++ ")"] - }
                    Just x -> [predLit $ Req x path set]

        f _ = []

-}



propagate :: Hite -> FuncName -> Reqs -> Reqs
propagate hite on reqs
    | isTrue reqs = predTrue
    | otherwise = predAnd $ res
        where
    
{-
    | otherwise = predAnd $ map reAll $ findCallsHistory hite f
    where
        f c@(Call (CallFunc n) args) | n == on = Just $ mapPredLit g reqs
            where
                g (Req (Var n "*") path opts) = predLit $ Req (fromJust $ callArg c pos) path opts
                    where pos = getArgPos on n hite
                g x = predLit x
        f _ = Nothing

        reAll (name, x) = predLit $ ReqAll name (mapPredLit f x)
            where
                f (Req a b c) = predLit $ Req (mapExpr g a) b c

                g (Var x y) = Var x 
                g x = x

        
-}
            orig = funcArgs $ getFunc hite on

            f rep (Req (Var var) path set) = predLit $ Req (fromJust $ lookup var rep) path set

            res = [predLit $ ReqAll (funcName func) $
                    predOr [predNot hite $ mcasePred hite cond, mapPredLit (f $ zip orig args) reqs] |
                    func <- funcs hite, let MCase alts = body func, MCaseAlt cond act <- alts,
                    (Call (CallFunc n) args) <- allExpr act, n == on]

