
module Hite.Inline(inline) where

import Hite.Type
import Hite.Normalise
import Maybe


inline :: Hite -> Hite
inline bad_hite = normalise $ mapExpr f $ mapExpr f $ mapExpr f hite
    where
        hite = normalise bad_hite
        res = inlineable hite
        
        f (Call (CallFunc x) xs)
                | x `elem` res && length xs == length args
                = mapExpr g body
            where
                Func _ args body _ = getFunc hite x
                ren = zip args xs
                g (Var x _) = fromJust $ lookup x ren
                g x = x

        f x = x


inlineable :: Hite -> [FuncName]
inlineable hite = map funcName $ filter canInline (funcs hite)
    where
        canInline func = f (body func) || funcName func == "o"
    
        f (Call (CallFunc _) xs) = all g xs
        f (Call x xs) = all g (x:xs)
        f (Make x xs) = all g xs
        f x = g x
        
        g (Var _ _) = True
        g (Sel x _) = g x
        g _ = False
