------------------------------------------------------------------
-- Searching in a grid of words for hidden words oriented in any of
-- the 8 possible directions.
-- Colin Runciman, May 1984 (this version, for tracing, Oct 1999)
------------------------------------------------------------------

module Soda3 where

-- * NEIL MODIFICATION
-- import List (transpose) 

transpose       :: [[a]] -> [[a]]
transpose []         = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:t) <- xss]) : transpose (xs : [ t | (h:t) <- xss])



main grid hidden = mapM (putStr.find) hidden
    where
    find word = word ++ " " ++ concat dirs ++ "\n"
        where
        dirs = map snd (
            filter (any (contains word) . fst)
                [(r,"right "), (d,"down "), (dr,"downright "), (ur,"upright ")]
            ++
            filter (any (contains drow) . fst) 
                [(r,"left "), (d,"up "), (dr,"upleft "), (ur,"downleft ")] )
        drow = reverse word
    r  = grid
    d  = transpose grid
    dr = diagonals grid
    ur = diagonals (reverse grid)

diagonals [r] = map (:[]) (reverse r)
diagonals (r:rs) = zipinit (reverse r) ([]:diagonals rs)

zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys
zipinit xs ys = ys

contains xs ys = any (prefix xs) (suffixes ys)

suffixes [] = []
suffixes xs = xs : suffixes (tail xs) 

prefix [] ys = True
prefix xs [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys
