

data Nat = S Nat | Z

inf = S inf

nat0 = Z
nat1 = S$Z
nat2 = S$S$Z
nat3 = S$S$S$Z
nat4 = S$S$S$S$Z

ack Z     n     = S n
ack (S m) Z     = ack m (S Z)
ack (S m) (S n) = ack m (ack (S m) n)


instance Show Nat where
    show Z = "Z"
    show (S x) = 'S' : show x
