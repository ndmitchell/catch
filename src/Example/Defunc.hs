
module Defunc where

main y xs = even2 y : map (f 1) xs


f :: Int -> Int -> Bool
f x y = x > y


odd2 :: Int -> Bool
odd2 x = x == 3

even2 = not . odd2
