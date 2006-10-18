
module Hill.PrimOp(evalPrim, primIntToInteger) where

import Hill.Type


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim "prim_LT_W" [Const (AInt a), Const (AInt b)] = Just $ mkBool $ a < b
evalPrim "prim_GT_W" [Const (AInt a), Const (AInt b)] = Just $ mkBool $ a > b
evalPrim _ _ = Nothing



mkBool True  = Make "True"  []
mkBool False = Make "False" []



intInteger = [("YHC.Primitive.primIntegerSub","prim_SUB_W")
             ,("YHC.Primitive.primIntegerLt","prim_LT_W")
             ]

primIntToInteger x = case lookup x intInteger of
                         Just y -> y
                         Nothing -> x
