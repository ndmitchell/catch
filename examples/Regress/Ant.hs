-- #CATCH _
-- NSC Open Assessment
-- A brute force solution to Q2 using Ant paths
-- Exam Number: 39250

module Ant where

import Prelude

main x = shortest x

isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

cat [] ys = ys
cat (x:xs) ys = if isPrefix (x:xs) ys then ys else x : cat xs ys

path [] = ""
path (n:ns) = cat n (path ns)

permutations [] = [[]]
permutations (x:xs) = concatMap (place x) (permutations xs)
  where place x [] = [[x]]
        place x (y:ys) = (x:y:ys) : map (y:) (place x ys)

shortest = foldr1 shorter . map path . permutations
  where shorter x y = if length x < length y then x else y
