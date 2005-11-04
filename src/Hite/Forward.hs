
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

import Hite.Type
import General


forward :: Hite -> Hite
forward h = h{funcs = map (forwardFunc h) (funcs h)}


forwardFunc :: Hite -> Func -> Func
forwardFunc h func = func{expr = forwardExpr h [] (expr func)}


forwardExpr :: Hite -> [(Expr, Expr)] -> Expr -> Expr
forwardExpr h xs v@Var{} = lookupDef v v xs
                                   
forwardExpr h xs (Case v@(Var name path) alts) = Case v (map f alts)
    where
        f (ctor, x) = (ctor, forwardExpr h ((v, v2):xs) x)
            where
                v2 = Make ctor $ map g $ ctorArgs $ getCtor ctor h
                g x = Var name (path ++ [x])


forwardExpr h xs (Call y ys) = Call (forwardExpr h xs y) (map (forwardExpr h xs) ys)
forwardExpr h xs (Make y ys) = Make y (map (forwardExpr h xs) ys)
forwardExpr h xs (CallFunc x) = CallFunc x
