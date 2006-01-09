
module Risers where

main x = risers x

risers :: Ord a => [a] -> [[a]]
risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = if undefined then (x:s):ss else [x]:(s:ss)
   where (s:ss) = risers (y:etc)
