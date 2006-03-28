
module Hite.ShortName(cmd, shortName) where

import Hite.Type
import List
import Maybe
import General.General

import Hite.Check

cmd = cmdHitePure (const shortName) "short-name"
            "Shorten names, if the module name is not required for disambiguation"



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
        
        g (CallFunc x) = CallFunc $ fromJustNote ("shortName(1), missing " ++ x) $ lookup x rename
        g x = x
        
        h func = func{funcName = fromJustNote "shortName(2)" $ lookup (funcName func) rename}
        
        makeShort x | '.' `elem` x = reverse $ takeWhile (/= '.') $ reverse x
        makeShort x = x

