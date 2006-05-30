
module Graph.Show where

import Graph.Type


instance Show Node where
    showList xs = showString $ unlines ("Graph:" : zipWith f [0..] xs)
        where f n x = show n ++ ": " ++ show x

    show (Node name edges rewrite) = name ++ " " ++ show edges ++ " " ++ show rewrite

instance Show Rewrite where
    show (Rewrite a b) = show a ++ " -> " ++ show b
    show (GraphEnd) = "GraphEnd"
    show (GraphBreak) = "GraphBreak"

instance Show GExp where
    show (GVar x) = "$" ++ x
    show (GCtor x xs) = "(" ++ x ++ concatMap ((' ':) . show) xs ++ ")"
    show (GFunc x xs) = show (GCtor x [xs])
    show (GStr x) = show x
    show (GFree) = "_"
