
-- If a function is a constant then inline it
-- If a function only calls constant functions, then inline it

module Hite.StringExpand(cmd) where

import Hite.Type


cmd = cmdHitePure (const stringExpand) "string-expand"
            "Replaces literal strings with a list of chars"


stringExpand :: Hite -> Hite
stringExpand hite = mapExpr f hite
    where
        f (Msg xs) = foldr f (Make "[]" []) xs
            where
                f c res = Make ":" [Make (charCtor c) [], res]
        
        f x = x
