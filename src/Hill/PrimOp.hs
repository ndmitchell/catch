
module Hill.PrimOp(evalPrim, primImports, primHaskell, primIntToInteger) where

import Hill.Type



primImports = ["System.IO", "Data.Char", "Foreign"]


data Op = OpIntIntInt  String (Int -> Int -> Int)
        | OpIntIntBool String (Int -> Int -> Bool)
        | OpIntInt     String (Int -> Int)
        | OpIntegerInt String (Integer -> Int)
        | OpIntegerIntegerBool String (Integer -> Integer -> Bool)
        | OpIntegerIntegerInteger String (Integer -> Integer -> Integer)


ops :: [(String,Op)]
ops = [("prim_LT_W",  OpIntIntBool "(<)" (<))
      ,("prim_GT_W",  OpIntIntBool "(>)" (>))
      ,("prim_EQ_W",  OpIntIntBool "(==)" (==))
      ,("prim_NE_W",  OpIntIntBool "(/=)" (/=))

      ,("prim_NEG_W", OpIntInt     "negate" negate)

      ,("prim_SUB_W", OpIntIntInt  "(-)" (-))
      ,("prim_ADD_W", OpIntIntInt  "(+)" (+))

      ,("prim_QUOT",  OpIntIntInt  "quot" quot)
      ,("prim_REM",   OpIntIntInt  "rem" rem)
      
      ,("YHC.Primitive.primIntSignum", OpIntInt "signum" signum)
      
      ,("YHC.Primitive.primIntFromInteger", OpIntegerInt "fromInteger" fromInteger)
      
      ,("YHC.Primitive.primIntegerLt", OpIntegerIntegerBool "(<)" (<))
      
      ,("YHC.Primitive.primIntegerSub", OpIntegerIntegerInteger "(-)" (-))
      ]

opsRaw = ["System.IO.stdout", "System.IO.stdin"]


evalPrim :: String -> [Expr] -> Maybe Expr
evalPrim name args = do
    res <- lookup name ops
    case (res, args) of
        (OpIntIntBool _ op, [Const (AInt a), Const (AInt b)]) -> Just $ mkBool $ op a b
        (OpIntIntInt  _ op, [Const (AInt a), Const (AInt b)]) -> Just $ mkInt  $ op a b
        (OpIntInt     _ op, [Const (AInt a)]) -> Just $ mkInt $ op a
        (OpIntegerInt _ op, [Const (AInteger a)]) -> Just $ mkInt $ op a
        _ -> Nothing



mkBool :: Bool -> Expr
mkBool x = Make (show x) []

mkInt :: Int -> Expr
mkInt = Const . AInt


intInteger = [("YHC.Primitive.primIntegerSub","prim_SUB_W")
             ,("YHC.Primitive.primIntegerLt","prim_LT_W")
             ]

primIntToInteger x = case lookup x intInteger of
                         Just y -> y
                         Nothing -> x


primHaskell "System.IO.hPutChar" = "\\h x -> io (System.IO.hPutChar h (chr x))"

primHaskell x = 
    case lookup x ops of
        Nothing | x `elem` opsRaw -> x
                | otherwise -> error $ "Hill.PrimOp, unrecognised " ++ x
        Just y -> case y of
            OpIntIntBool s _ -> "(" ++ s ++ " :: Int -> Int -> Bool)"
            OpIntIntInt  s _ -> "(" ++ s ++ " :: Int -> Int -> Int)"
            OpIntInt     s _ -> "(" ++ s ++ " :: Int -> Int)"
            OpIntegerInt s _ -> "(" ++ s ++ " :: Integer -> Int)"
            OpIntegerIntegerBool s _ -> "(" ++ s ++ " :: Integer -> Integer -> Bool)"
            OpIntegerIntegerInteger s _ -> "(" ++ s ++ " :: Integer -> Integer -> Integer)"
