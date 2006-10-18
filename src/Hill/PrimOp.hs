
module Hill.PrimOp(evalPrim, primIntToInteger) where

import Hill.Type


biops :: [(String, Int -> Int -> Expr)]
biops = [("prim_LT_W", mkBool2 (<))
        ,("prim_GT_W", mkBool2 (>))
        ,("prim_SUB_W", mkInt2 (-))
        ]


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim name [Const (AInt a), Const (AInt b)] = do
        res <- lookup name biops
        return $ res a b

evalPrim _ _ = Nothing



mkBool2 f a b = Make (show $ f a b) []

mkInt2 f a b = Const $ AInt $ f a b


intInteger = [("YHC.Primitive.primIntegerSub","prim_SUB_W")
             ,("YHC.Primitive.primIntegerLt","prim_LT_W")
             ]

primIntToInteger x = case lookup x intInteger of
                         Just y -> y
                         Nothing -> x
