-- !!! the ultra-notorious "nfib 30" does w/ Floats
--
module Rfib (main) where

main = print (nfib 30)

nfib :: Double -> Double
nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2) + 1

