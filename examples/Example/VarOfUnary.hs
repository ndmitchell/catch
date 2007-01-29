-- Extracted from Taut

module VarOfUnary where

data Prop = Lit Bool
          | Var Char
          | Not Prop
          | Imp Prop


main x = varOf x

varOf :: Prop -> Char
varOf (Var v)   = v
varOf (Not p)   = varOf p
varOf (Imp p) = varOf p

