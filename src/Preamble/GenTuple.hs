
import List

makeTuple :: Int -> String
makeTuple n =
        "data Tup" ++ show n ++ concatMap (\x -> " a" ++ show x) [1..n] ++
        " = Tup" ++ show n ++ " {" ++ concat (intersperse ", " fields) ++ "}"
    where
        fields = map (\x -> "tup" ++ show n ++ "_" ++ show x ++ " :: a" ++ show x) [1..n]

