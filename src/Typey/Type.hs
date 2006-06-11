
module Typey.Type where

import Hite

-- TYPE STUFF

type FuncM = [(FuncName, FuncT)]
type DataM a = [(DataName, DataT a)]

data FuncT = FuncT Int [LargeT] LargeT
data DataT a = DataT Int [CtorT a]
data CtorT a = CtorT String [a]
data LargeT = FreeL Int | CtorL String [LargeT]
data SmallT = FreeS Int | Self


class PlayLargeT a where
    mapLargeT :: (LargeT -> LargeT) -> a -> a
    
instance PlayLargeT LargeT where
    mapLargeT f x = f $ case x of
                        CtorL name xs -> CtorL name $ map (mapLargeT f) xs
                        x -> x

instance PlayLargeT FuncT where
    mapLargeT f (FuncT n xs x) = FuncT n (map (mapLargeT f) xs) (mapLargeT f x)



-- SUBTYPE STUFF

data Subtype = Subtype [UCtor] [UCtor] [Subtype] [Subtype]
             | Top | Bot

data UCtor = UCtor String | UVar Int
