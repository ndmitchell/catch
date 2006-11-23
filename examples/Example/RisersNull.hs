
module RisersNull where

main x = any null $ mainR x

mainR :: [Int] -> [[Int]]
mainR x = risers x

risers :: Ord a => [a] -> [[a]]
risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = if x <= y then (x:s):ss else [x]:(s:ss)
   where (s:ss) = risers (y:etc)
