

module Checker.Propagate(propagate) where

import Hite
import Constraint
import List
import Options


propagate :: Hite -> Req -> Reqs
propagate a b | propagateSimp = propagateSimple a b

propagate hite r@(Req (Var arg name) path set Nothing) = 
        predAnd $ concatMap (f predFalse . body) $ funcs $ callOne hite
    where
        pos = getArgPos name arg hite
        
        
        f :: Reqs -> Expr -> [Reqs]
        f hist c@(Call (CallFunc n) args) | n == name =
                concatMap (f hist) args ++
                case callArg c pos of
                    Nothing -> error $ "unsaturated: " ++ show (length args, c) -- [predOr [hist, predFalse]] -- unsaturued use of partial app
                    Just x -> [predOr [hist, predLit $ Req x path set Nothing]]
                    
        f hist (Case on alts) = concatMap g alts
            where
                allCtor = map ctorName $ ctors $ getDataFromCtor (fst $ head alts) hite
                g (typ, expr) = f (predOr [hist, predLit $ Req on pathLambda (allCtor \\ [typ]) Nothing]) expr
                
        
        f hist x = concatMap (f hist) $ case x of
            Call x xs -> x : xs
            Make _ xs -> xs
            Sel x _ -> [x]
            _ -> []



propagate hite req = error $ "Internal error, complex pattern as subject of req: " ++ show req



-- make all CallFunc's be in the LHS or a Call
callOne :: Hite -> Hite
callOne hite = mapExpr f hite
    where
        f (CallFunc x) = Call (CallFunc x) []
        f (Call (Call (CallFunc x) []) ys) = Call (CallFunc x) ys
        f x = x



propagateSimple :: Hite -> Req -> Reqs
propagateSimple hite (Req (Var arg name) path set Nothing) = 
        predAnd $ concatMap f $ allExpr $ callOne hite
    where
        pos = getArgPos name arg hite

        f c@(Call (CallFunc n) _) | n == name = 
                case callArg c pos of
                    Nothing -> [predFalse ] {- $ "Unsaturated use of partial function, " ++ name ++
                                           " (wanted " ++ show pos ++ ")"] -}
                    Just x -> [predLit $ Req x path set Nothing]

        f _ = []

