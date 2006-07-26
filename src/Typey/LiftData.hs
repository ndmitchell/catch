
module Typey.LiftData(liftData, getDatas) where

import Hite
import Typey.Type


injectData = [("Bool",DataT 0 [CtorT "True" [], CtorT "False" []])
             ,("[]",DataT 1 [CtorT "[]" [], CtorT ":" [FreeS 0, Self]])
             ,("Compare",DataT 0 $ map (`CtorT` []) ["EQ","LT","GT"])
             ] ++
             map makeTuple [0..8]
    where
        makeTuple n = (tup, DataT n [CtorT tup [FreeS (i-1) | i <- [1..n]]])
            where
                tup = "Tup" ++ show n

liftData :: Hite -> FuncM -> (DataM SmallT, FuncM)
liftData hite funcs = (getDatas hite, funcs)


getDatas :: Hite -> DataM SmallT
getDatas x = injectData
