
module Typey.LiftData(liftData, getDatas) where

import Typey.Type


injectData = [("Bool",DataT 0 [CtorT "True" [], CtorT "False" []])
             ,("[]",DataT 1 [CtorT "[]" [], CtorT ":" [FreeS 0, Self]])
             ]

liftData :: DataM LargeT -> FuncM -> (DataM SmallT, FuncM)
liftData datas funcs = (injectData, funcs)


getDatas :: DataM SmallT
getDatas = injectData
