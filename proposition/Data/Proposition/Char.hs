
module Data.Proposition.Char where

import Data.Proposition.Internal


data PropChar = PropChar Bool Char
                deriving (Show, Eq, Ord)


instance PropNot PropChar where
    litNot (PropChar b c) = PropChar (not b) c


instance PropLit PropChar where
    xs ?=> (PropChar b c) = if null res then Just (head res /= b) else Nothing
        where res = [b1 /= b2 | (PropChar b2 c1,b1) <- xs, c1 == c]

    (PropChar b1 c1) ?\/ (PropChar b2 c2) | c1 == c2 && b1 == not b2 = Literal True
                                          | otherwise = None
    
    (PropChar b1 c1) ?/\ (PropChar b2 c2) | c1 == c2 && b1 == not b2 = Literal False
                                          | otherwise = None


instance PropLit Char
