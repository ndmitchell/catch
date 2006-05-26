
module Graph.Show where

import Graph.Type


instance Show Graph where
    show (Graph xs) = unlines ("Graph:" : zipWith f [0..] xs)
        where
            f n x = show n ++ ": " ++ show x

instance Show Node where
    show (Node edges rewrite) = show edges ++ " " ++ show rewrite

instance Show Rewrite where
    show (Rewrite a b) = show a ++ " -> " ++ show b

instance Show GExp where
    show (GFree x) = "$" ++ x
    show (GCtor x xs) = "(" ++ x ++ concatMap ((' ':) . show) xs ++ ")"
    show (GFunc x xs) = show (GCtor x [xs])
