
module Transform.Reform(reform) where

import Transform.Driver
import Transform.Type


reform :: IHite -> IHite
reform ihite = error $ show $ calcArgs ihite 




data Weight = Weak | Normal | Strong
              deriving (Eq,Show)


calcArgs :: IHite -> [(FuncName,[Weight])]
calcArgs ihite = []
