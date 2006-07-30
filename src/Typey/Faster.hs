
-- some things take quite a while and are very common
-- solve them super fast

module Typey.Faster(canFastEval, fastEval) where

import Hite
import Typey.Abstract

fakeFast = [] -- ["subset","dif","insert"]


canFastEval :: FuncName -> Bool
canFastEval x = x `elem` (["map", "or"] ++ fakeFast)


fastEval :: (Eq a, Show a) => ([Abstract a] -> IO (Abstract a)) -> FuncName -> [Abstract a] -> IO (Abstract a)

-- map is perfectly specified
fastEval eval "map" [f, AbsAny] = fastEval eval "map" [f, full]
    where full = List False [Bit True, Bit True, AbsAny] [Bit True, Bit True, AbsAny]
fastEval eval "map" [f, AbsVoid] = return $ AbsVoid
fastEval eval "map" [f, List b [n,c,x] [ns,cs,xs]] = do
    x2 <- eval [f,x]
    xs2 <- eval [f,xs]
    return $ List b [n,c,x2] [ns,cs,xs2]

-- or is perfectly specified
fastEval eval "or" [AbsVoid] = return AbsVoid
fastEval eval "or" [List b [Bit n ,Bit c ,v ]
                           [Bit ns,Bit cs,vs]] = return res
    where
        List b1 [Bit f ,Bit t ] [] = lift v
        List b2 [Bit fs,Bit ts] [] = lift vs
        
        lift (x@List{}) = x
        lift AbsAny = List False [Bit True, Bit True] []
        lift AbsVoid = List False [Bit False, Bit False] []
        lift x = error $ "fastEval or failed with lift of: " ++ show x
        
                           
        res = List (b || b1 || b2)
                   [(Bit (n || (not t && not ts)))
                   ,(Bit (t || ts))]
                   []
 
 
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
          

fastEval eval x _ | x `elem` fakeFast = return AbsAny


fastEval _ f xs = error $ "Can't fastEval " ++ f ++ " " ++ show xs
