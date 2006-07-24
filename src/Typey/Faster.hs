
-- some things take quite a while and are very common
-- solve them super fast

module Typey.Faster(canFastEval, fastEval) where

import Hite
import Typey.Abstract


canFastEval :: FuncName -> Bool
canFastEval x = x `elem` ["map"] -- , "++"]


fastEval :: (Eq a, Show a) => ([Abstract a] -> IO (Abstract a)) -> FuncName -> [Abstract a] -> IO (Abstract a)

-- map is perfectly specified
fastEval eval "map" [f, AbsVoid] = return $ AbsVoid
fastEval eval "map" [f, List b [n,c,x] [ns,cs,xs]] = do
    x2 <- eval [f,x]
    xs2 <- eval [f,xs]
    return $ List b [n,c,x2] [ns,cs,xs2]
 
-- ++ is reasonable, but a little bit general in some cases
-- mainly in the x cases, I think
-- do not use, since not really a speed up and a loss of accuracy
fastEval eval "++" [List b1 [Bit n1,Bit c1,x1] [Bit ns1,Bit cs1,xs1]
                   ,List b2 [Bit n2,Bit c2,x2] [Bit ns2,Bit cs2,xs2]] = return $
    List (b1 || b2)
        [Bit (n1 && n2)
        ,Bit (c1 || cs1 || c2 || cs2)
        ,(unionAbs $ [x1|c1] ++ [xs1|cs1] ++ [x2|c2] ++ [xs2|cs2])]
        [Bit ((n1 || ns1) && (n2 || ns2))
        ,Bit (cs1 || c2 || cs2)
        ,(unionAbs [x1,xs1,x2,xs2])]
          

fastEval _ f xs = error $ "Can't fastEval " ++ f ++ " " ++ show xs
