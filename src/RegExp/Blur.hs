
module RegExp.Blur(blur) where

import RegExp.Type
import General.Similar

-- the output of blur must be a finite set
-- assuming a finite input
blur :: Eq a => RegExp a -> RegExp a
blur x = f x
    where
        f (RegKleene x) = regKleene (f x)
        f (RegUnion x) = regUnion (map f x)
        f (RegConcat x) = regConcat (g x)
        f x = x
        
        g (a:b:c:d) | a ~= b && b ~= c =
            a : b : RegKleene c :
            (
                if not (null d) && (head d ~= RegKleene c) then
                    tail d
                else
                    d
            )
        g (x:xs) = x : g xs
        g [] = []

