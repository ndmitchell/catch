
module RegExp.Enum where

import RegExp.Type


regExpEnum :: (Eq a, Show a) => [a] -> Int -> [RegExp a]
regExpEnum terms 0 = [regOmega]
regExpEnum terms 1 = regLambda : map regLit terms ++ map (regKleene . regLit) terms
regExpEnum terms n = both ++ map regKleene both
    where
        items = map (regExpEnum terms) [1..n-1]
        pairs = concat $ map allZips $ zip items (reverse items)
        allZips (a, b) = [[c, d] | c <- a, d <- b]
        unions = map regUnion  pairs
        concts = map regConcat pairs
        both = unions ++ concts




bisimilarStream :: (Eq a, Show a) => [RegExp a] -> [RegExp a]
bisimilarStream xs = f [] xs
    where
        f _ [] = []
        f done (x:xs) | any (== x) done = f done xs
                      | otherwise = x : f (x:done) xs
                      



groupyBy :: (Eq a, Show a) => (a -> a -> Bool) -> [a] -> [[a]]
groupyBy f [] = []
groupyBy f (x:xs) = g (groupyBy f xs)
    where
        g ((y:ys):zs) | f x y = (x:y:ys):zs
                      | otherwise = (y:ys) : g zs
        g [] = [[x]]
