
{-|
    Forward propogate variable information
    
    For example:
        case x of
            Nil -> x
            Cons -> f x
        --------------------
        case x of
            Nil -> Nil
            Cons -> f (Cons x.hd x.tl)
    
    Inside a case statement on variable x, you may not refer to x directly
-}

module Hite.Forward(forward) where


forward :: Hite -> Hite
forward h = h
