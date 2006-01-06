
module Hite.Inline(inline) where

import Hite.Type


inline :: Hite -> Hite
inline hite = mapExpr f hite
    where
        res = inlineable hite
        
        f (CallFunc x) = case lookup x res of
                             Just k -> CallFunc k
                             Nothing -> CallFunc x
        f x = x



inlineable :: Hite -> [(FuncName, FuncName)]
inlineable hite = concatMap f (funcs hite)
    where
        f (Func name args1 (Call (CallFunc x) args2) _) | and (zipWith g args1 args2) = [(name, x)]
        f _ = []
        
        g s1 (Var s2 _) = s1 == s2
        g _ _ = False


