
module Tails where

main x = tails x

tails x = foldr tails2 [[]] x

tails2 x y = (x:head y) : y
