
module List where

main x = list x

list [] = []
list (x:xs) = x : list xs
