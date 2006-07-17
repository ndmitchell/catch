
module HoType where

main :: Bool -> (a -> b) -> a -> a -> b
main True f x y = f x
main False f x y = f y
