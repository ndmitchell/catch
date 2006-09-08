
module Transform.Reform(reform) where

import Transform.Driver
import Transform.Type
import General.General


reform :: IHite -> IHite
reform ihite = error $ show $ calcArgs ihite 




data Weight = Weak | Normal | Strong
              deriving (Eq,Show)


calcArgs :: IHite -> [(FuncName,[Weight])]
calcArgs ihite@(IHite datas funcs) = fixp f base
    where
        base = [(funcName func, replicate (length $ funcArgs func) Weak) | func <- funcs]
        
        f know = map (g know) know
        
        g know x = x

        

fixp f x = if x == x2 then x else fixp f x2
    where x2 = f x
