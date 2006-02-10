
module Hite.Inline(inline) where

import Hite.Type
import Hite.Normalise
import Maybe


inline :: Hite -> Hite
inline hite = mapExpr f hite
    where
        res = inlineable (normalise hite)
        
        f (Call (CallFunc x) xs)
                | x `elem` res && length xs == length args
                = mapExpr g body
            where
                Func _ args body _ = getFunc x hite
                ren = zip args xs
                g (Var x _) = fromJust $ lookup x ren
                g x = x

        f x = x


inlineable :: Hite -> [FuncName]
inlineable hite = map funcName $ filter (f . body) (funcs hite)
    where
        f (Call (CallFunc _) xs) = all g xs
        f (Call x xs) = all g (x:xs)
        f x = g x
        
        g (Var _ _) = True
        g (Sel x _) = g x
        g _ = False
