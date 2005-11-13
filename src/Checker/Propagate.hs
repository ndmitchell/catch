

module Checker.Propagate(propagate) where

import Hite
import Constraint


propagate :: Hite -> Req -> [Either Req String]
propagate hite (Req name (Var arg []) path set) = 
        concatMap f [(func, e) | Func func _ body <- funcs hite, e <- allExprCall body]
    where
        pos = getArgPos name arg hite
        
        f (func, CallFunc n) | n == name =
            [Right $ "Higher order use of partial function, " ++ name ++ " inside " ++ func]
        
        f (func, c@(Call (CallFunc n) _)) | n == name = 
                case callArg c pos of
                    Nothing -> [Right $ "Unsaturated use of partial function, " ++ name ++
                                        " inside " ++ func ++ " (wanted " ++ show pos ++ ")"]
                    Just x -> [Left $ Req func x path set]
            where
                a = callArg c pos

        f _ = []

propagate hite req = [Right $ "Internal error, complex pattern as subject of req: " ++ show req]


allExprCall :: Expr -> [Expr]
allExprCall x = x : concatMap allExprCall (case x of
        Call (CallFunc _) xs -> xs
        Call x xs -> x : xs
        Make _ xs -> xs
        Case _ xs -> map snd xs
        _ -> []
    )
