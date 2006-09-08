
module Sort where

main = putStrLn $ sort "Hello World!"


sort (x:xs) = filter (< x) xs ++ [x] ++ filter (>= x) xs
sort [] = []
