
module Typey.LiftData(liftData, getDatas) where

import Hite
import Typey.Type
import Data.List
import Data.Maybe


liftData :: Hite -> FuncM -> (DataM SmallT, FuncM)
liftData hite funcs = (getDatas hite, funcs)


getDatas :: Hite -> DataM SmallT
getDatas hite = map f (datas hite)
    where
        f (Data dname ctrs free) = (dname, DataT (length free) (map g ctrs))
            where
                g (Ctor cname _ tys) = CtorT cname (map h tys)
        
                h (TyFree n) = FreeS $ fromJust $ elemIndex n free 
                h (TyCon x xs) | x == dname && map TyFree free == xs = Self
                h x = error $ "getDatas, " ++ dname ++ ", can't make small " -- ++ show x
