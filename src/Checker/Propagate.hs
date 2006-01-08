

module Checker.Propagate(propagate) where

import Hite
import Constraint


propagate :: Hite -> Req -> Reqs
propagate hite (Req (Var arg name) path set) = 
        predAnd $ concatMap f $ allExpr $ callOne hite
    where
        pos = getArgPos name arg hite

        f c@(Call (CallFunc n) _) | n == name = 
                case callArg c pos of
                    Nothing -> [predFalse ] {- $ "Unsaturated use of partial function, " ++ name ++
                                           " (wanted " ++ show pos ++ ")"] -}
                    Just x -> [predLit $ Req x path set]

        f _ = []


propagate hite req = error $ "Internal error, complex pattern as subject of req: " ++ show req



-- make all CallFunc's be in the LHS or a Call
callOne :: Hite -> Hite
callOne hite = mapExpr f hite
    where
        f (CallFunc x) = Call (CallFunc x) []
        f (Call (Call (CallFunc x) []) ys) = Call (CallFunc x) ys
        f x = x
