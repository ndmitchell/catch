

data Cons
data NotCons
newtype List a t = List [a]
    deriving Show


nil :: List a NotCons
nil = List []

cons :: a -> [a] -> List a Cons
cons a as = List (a:as)

fromList :: [a] -> List a NotCons
fromList xs = List xs


safeHead :: List a Cons -> a
safeHead (List (a:as)) = a

