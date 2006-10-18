
module Hill.PrimOp(evalPrim, primIntToInteger) where

import Hill.Type



data BiOp = IntOp String (Int -> Int -> Int)
          | BoolOp String (Int -> Int -> Bool)


biops :: [(String,BiOp)]
biops = [("prim_LT_W", BoolOp "(<)" (<))
        ,("prim_GT_W", BoolOp "(>)" (>))
        ,("prim_SUB_W", IntOp "(-)" (-))
        ]


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim name [Const (AInt a), Const (AInt b)] = do
        res <- lookup name biops
        return $ case res of
            BoolOp _ op -> Make (show $ op a b) []
            IntOp _ op -> Const $ AInt $ op a b

evalPrim _ _ = Nothing



intInteger = [("YHC.Primitive.primIntegerSub","prim_SUB_W")
             ,("YHC.Primitive.primIntegerLt","prim_LT_W")
             ]

primIntToInteger x = case lookup x intInteger of
                         Just y -> y
                         Nothing -> x
