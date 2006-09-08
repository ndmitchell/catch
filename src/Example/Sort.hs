
module Sort where

main = putStrLn $ sort "Hello World!"


sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)
sort [] = []
