
-- A safe version of Risers

module RisersSafe where

main :: [Int] -> [[Int]]
main x = risers x

risers :: Ord a => [a] -> [[a]]
risers [] = []
risers (x:xs) = a:b
    where (a,b) = risers' x xs


risers' :: Ord a => a -> [a] -> ([a], [[a]])
risers' x [] = ([x], [])
risers' x (y:etc) = if x <= y then (x:s,ss) else ([x],s:ss)
    where (s,ss) = risers' y etc
