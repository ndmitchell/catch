
-- some things take quite a while and are very common
-- solve them super fast

module Typey.Faster(canFastEval, fastEval) where

import Hite
import Typey.Abstract


canFastEval :: FuncName -> Bool
canFastEval x = x `elem` ["map"]


fastEval :: Show a => ([Abstract a] -> IO (Abstract a)) -> FuncName -> [Abstract a] -> IO (Abstract a)
fastEval eval "map" [f, AbsVoid] = return $ AbsVoid
fastEval eval "map" [f, List b [n,c,x] [ns,cs,xs]] = do
    x2 <- eval [f,x]
    xs2 <- eval [f,xs]
    return $ List b [n,c,x2] [ns,cs,xs2]

fastEval _ f xs = error $ "Can't fastEval " ++ f ++ " " ++ show xs
