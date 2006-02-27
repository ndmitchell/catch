
module Hite.ShortName(shortName) where

import Hite.Type
import List
import Maybe

shortName :: Hite -> Hite
shortName hite = mapFunc h $ mapExpr g hite
    where
        names = map funcName (funcs hite)
        shorts = map makeShort names
        badshorts = nub (shorts \\ nub shorts)
        goodshorts = nub shorts \\ badshorts
        
        rename = zipWith f names shorts
        f a b | b `elem` goodshorts = (a,b)
              | otherwise           = (a,a)
        
        g (CallFunc x) = CallFunc $ fromJust $ lookup x rename
        g x = x
        
        h func = func{funcName = fromJust $ lookup (funcName func) rename}
        
        makeShort x | '.' `elem` x = reverse $ takeWhile (/= '.') $ reverse x
        makeShort x = x

