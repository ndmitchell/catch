
module Hill.PrimOp(evalPrim, primImports, primHaskell, primIntToInteger) where

import Hill.Type



primImports = ["System.IO", "Data.Char", "Foreign"]


data BiOp = IntOp String (Int -> Int -> Int)
          | BoolOp String (Int -> Int -> Bool)


biops :: [(String,BiOp)]
biops = [("prim_LT_W", BoolOp "(<)" (<))
        ,("prim_GT_W", BoolOp "(>)" (>))
        ,("prim_EQ_W", BoolOp "(==)" (==))
        ,("prim_SUB_W", IntOp "(-)" (-))
        ,("prim_ADD_W", IntOp "(+)" (+))
        ,("prim_QUOT", IntOp "quot" quot)
        ,("prim_REM", IntOp "rem" rem)
        ]


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim name [Const (AInt a), Const (AInt b)] = do
        res <- lookup name biops
        return $ case res of
            BoolOp _ op -> Make (show $ op a b) []
            IntOp _ op -> Const $ AInt $ op a b

evalPrim "YHC.Primitive.primIntFromInteger" [Const (AInteger x)] = Just $ Const $ AInt $ fromInteger x

evalPrim _ _ = Nothing



intInteger = [("YHC.Primitive.primIntegerSub","prim_SUB_W")
             ,("YHC.Primitive.primIntegerLt","prim_LT_W")
             ]

primIntToInteger x = case lookup x intInteger of
                         Just y -> y
                         Nothing -> x


primHaskell "System.IO.hPutChar" = "\\h x -> io (System.IO.hPutChar h (chr x))"
primHaskell "prim_NEG_W" = "\\x -> ((0 :: Int) - x)"
primHaskell x = 
    case lookup x biops of
        Nothing -> x
        Just y -> 
            case y of
                BoolOp s _ -> "\\a b -> bool ((" ++ s ++ " :: Int -> Int -> Bool) a b)"
                IntOp  s _ -> "(" ++ s ++ " :: Int -> Int -> Int)"
