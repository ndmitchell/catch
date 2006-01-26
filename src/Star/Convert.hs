
module Star.Convert(starToRegExp) where

import RegExp.Type
import Star.Type


starToRegExp :: (Show a, Eq a) => Star a -> RegExp a
starToRegExp x = case x of
        StarCon xs -> regConcat (map starToRegExp xs)
        StarUni xs -> regUnion  (map starToRegExp xs)
        StarLit x  -> regLit x
        Omega      -> regOmega
        Lambda     -> regLambda
        Star x z b -> f (starToRegExp x) z b
    where
        f x (z:zs) b = regConcat $ replicate z x ++ g x z zs ++ [regKleene x | b]
        
        g x lst [] = []
        g x lst (z:zs) = regUnion [regLambda, regConcat (replicate (z-lst) x)] : g x z zs
