
module Dictionary where


main :: Int -> Int -> [Bool]
main a b = test a b : []


test a b = a == b && a < b
