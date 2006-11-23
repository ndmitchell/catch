
module Show where

data Nat = Z | S Nat deriving Show

main x = show (x :: Nat)
