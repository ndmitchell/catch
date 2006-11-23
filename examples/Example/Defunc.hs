
module Defunc where

main y xs z = even2 y : func3 z y y : map (f 1) xs


f :: Int -> Int -> Bool
f x y = x > y


odd2 :: Int -> Bool
odd2 x = x == 3

even2 = not . odd2



func1 :: Int -> Int -> Bool
func1 = (==)

func2 :: Int -> Int -> Bool
func2 = (<=)


func3 :: Bool -> Int -> Int -> Bool
func3 True = func1
func3 False = func2
