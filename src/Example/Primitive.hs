-- check that you don't cut out primitives altogether

module Primitive where

main :: [Int] -> Int
main x = head x + head x


