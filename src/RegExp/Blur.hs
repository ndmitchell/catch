
module RegExp.Blur(blur) where

import RegExp.Type

-- the output of blur must be a finite set
-- assuming a finite input
blur :: Eq a => RegExp a -> RegExp a
blur x = f x
    where
        f (RegKleene x) = regKleene (f x)
        f (RegUnion x) = regUnion (map f x)
        f (RegConcat x) = regConcat (g x)
        f x = x
        
        g (a : RegKleene b : c) | a == b = g (RegKleene b : a : c)
        
        g (RegKleene a : b : c : d : e) | a == b && b == c && c == d =
            g (RegKleene a : c : d : e)
        
        g (a:b:c:d) | a == b && b == c =
            g (RegKleene a : b : c : d)

        g (x:xs) = x : g xs
        g [] = []

