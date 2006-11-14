
module Hite.ShortName(cmd, shortName) where

import Hite.Type
import List
import Maybe
import Safe

cmd = cmdHitePure (const shortName) "short-name"
            "Shorten names, if the module name is not required for disambiguation"



shortName :: Hite -> Hite
shortName hite = Hite (map h2 ds) (map h1 fs)
    where
        Hite ds fs = mapExpr g hite
    
        names = map funcName (funcs hite)
        shorts = map makeShort names
        badshorts = nub (shorts \\ nub shorts)
        goodshorts = nub shorts \\ badshorts
        
        rename = zipWith f names shorts
        f a b | b `elem` goodshorts = (a,b)
              | otherwise           = (a,a)
        
        g (CallFunc x) = CallFunc $ lookupJustNote ("shortName(1), missing " ++ x) x rename
        g (Make x xs) = Make (makeShort x) xs
        g (Case on alts) = Case on [(makeShort a,b) | (a,b) <- alts]
        g x = x
        
        h1 func = func{funcName = lookupJustNote "shortName(2)" (funcName func) rename}
        h2 (Data a b c) = Data (makeShort a) [Ctor (makeShort d) e (map h3 f) | Ctor d e f <- b] c
        h3 (TyCon x xs) = TyCon (makeShort x) (map h3 xs)
        h3 x = x
        
        makeShort x | '.' `elem` x = if null res then tail $ takeWhile (== '.') $ reverse x else res
            where res = reverse $ takeWhile (/= '.') $ reverse x
        makeShort x = x

