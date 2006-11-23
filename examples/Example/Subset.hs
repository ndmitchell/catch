
-- from Adjoxo
module Subset where

type Region = [Int]

main x y = subset x y

dif :: Region -> Region -> Region
dif [] ys = []
dif xs [] = xs
dif xs@(x:xs') ys@(y:ys') =
  case compare x y of
  LT -> x : dif xs' ys
  EQ ->     dif xs' ys'
  GT ->     dif xs  ys'

subset :: Region -> Region -> Bool
subset xs ys = null (dif xs ys)
