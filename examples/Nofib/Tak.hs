-- code of unknown provenance (partain 95/01/25)

module Tak where

tak :: Int -> Int -> Int -> Int

tak x y z = if not(y < x) then z
       else tak (tak (x-1) y z)
		(tak (y-1) z x)
		(tak (z-1) x y)

main x y z = print (tak x y z)
