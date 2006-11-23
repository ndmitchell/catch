
module Tree where

data Tree a = Branch {left :: Tree a, right :: Tree a}
			| Node a
			| Bomb


main x = f x

f (Branch l r) = f l ++ f r
f (Node a) = [a]
