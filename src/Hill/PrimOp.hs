
module Hill.PrimOp(evalPrim) where

import Hill.Type


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim "prim_LT_W" [Const (AInt a), Const (AInt b)] = Just $ mkBool $ a < b
evalPrim "prim_GT_W" [Const (AInt a), Const (AInt b)] = Just $ mkBool $ a > b
evalPrim _ _ = Nothing



mkBool True  = Make "True"  []
mkBool False = Make "False" []
