
module Subst.Blur(substBlur) where

import Subst.Type


substBlur :: SExp -> SExp
substBlur x | not (isConcrete x) = error "substBlur called on non-concrete"
            | otherwise = f 3 x
    where
        f 0 x = SFree
        f n (SCtor x xs) = SCtor x (map (f (n-1)) xs)
        f n (SChoice xs) = SChoice (map (f (n-1)) xs)
        f n x = x
