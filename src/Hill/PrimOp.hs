
module Hill.PrimOp(evalPrim, primImports, primHaskell, primIntToInteger) where

import Hill.Type
import Data.Char



primImports = ["System.IO", "Data.Char", "Foreign"]


data Op = OpIntIntInt  String (Int -> Int -> Int)
        | OpIntIntBool String (Int -> Int -> Bool)
        | OpIntInt     String (Int -> Int)
        | OpIntegerInt String (Integer -> Int)
        | OpIntegerIntegerBool String (Integer -> Integer -> Bool)
        | OpIntegerIntegerInteger String (Integer -> Integer -> Integer)


ops :: [(String,Op)]
ops = [("LT_W",  OpIntIntBool "(<)" (<))
      ,("GT_W",  OpIntIntBool "(>)" (>))
      ,("EQ_W",  OpIntIntBool "(==)" (==))
      ,("NE_W",  OpIntIntBool "(/=)" (/=))

      ,("NEG_W", OpIntInt     "negate" negate)

      ,("SUB_W", OpIntIntInt  "(-)" (-))
      ,("ADD_W", OpIntIntInt  "(+)" (+))

      ,("QUOT",  OpIntIntInt  "quot" quot)
      ,("REM",   OpIntIntInt  "rem" rem)
      
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
        (OpIntIntBool _ op, [Const (AInt  a), Const (AInt  b)]) -> Just $ mkBool $ op a b
        (OpIntIntBool _ op, [Const (AChar a), Const (AChar b)]) -> Just $ mkBool $ op (ord a) (ord b)
        (OpIntIntInt  _ op, [Const (AInt a), Const (AInt b)]) -> Just $ mkInt  $ op a b
        (OpIntInt     _ op, [Const (AInt a)]) -> Just $ mkInt $ op a
        (OpIntegerInt _ op, [Const (AInteger a)]) -> Just $ mkInt $ op a
        (OpIntegerIntegerBool _ op, [Const (AInteger a), Const (AInteger b)]) -> Just $ mkBool $ op a b
        (OpIntegerIntegerInteger _ op, [Const (AInteger a), Const (AInteger b)]) -> Just $ mkInteger $ op a b
        _ -> Nothing



mkBool :: Bool -> Expr
mkBool x = Make (show x) []

mkInt :: Int -> Expr
mkInt = Const . AInt

mkInteger :: Integer -> Expr
mkInteger = Const . AInteger


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
