
module Hill.ShortName(shortName, cmdsShortName) where

import Hill.Type
import List
import Maybe
import General.General

cmdsShortName = [hillCmdPure "short-name" (const shortName)]



shortName :: Hill -> Hill
shortName hill = Hill (map h2 ds) (map h1 fs)
    where
        Hill ds fs = mapOverHill g hill
    
        names = map funcName (funcs hill)
        shorts = map makeShort names
        badshorts = nub (shorts \\ nub shorts)
        goodshorts = nub shorts \\ badshorts
        
        rename = zipWith f names shorts
        f a b | b `elem` goodshorts = (a,b)
              | otherwise           = (a,a)
        
        g (Fun x) = Fun $ fromJustNote ("shortName(1), missing " ++ x) $ lookup x rename
        g (Call x xs) = Call (fromJustNote ("shortName(1), missing " ++ x) $ lookup x rename) xs
        g (Make x xs) = Make (makeShort x) xs
        g (Const x) = Const $ g2 x
        g (Case on alts) = Case on (map g3 alts)
        g x = x
        
        g2 (ACtor x) = ACtor $ makeShort x
        g2 x = x
        
        g3 (Default x) = Default x
        g3 (Alt x y) = Alt (g2 x) y
        
        
        h1 func = func{funcName = fromJustNote "shortName(2)" $ lookup (funcName func) rename}
        h2 (Data a b c) = Data (makeShort a) [Ctor (makeShort d) e (map h3 f) | Ctor d e f <- b] c
        h3 (TyCon x xs) = TyCon (makeShort x) (map h3 xs)
        h3 x = x
        
        makeShort x | '.' `elem` x = if null res then tail $ takeWhile (== '.') $ reverse x else res
            where res = reverse $ takeWhile (/= '.') $ reverse x
        makeShort x = x

