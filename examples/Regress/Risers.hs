-- #CATCH _

module Risers where

main :: [Int] -> [[Int]]
main x = risers x

risers :: Ord a => [a] -> [[a]]
risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = if x <= y then (x:s):ss else [x]:(s:ss)
   where (s:ss) = risers (y:etc)
