
module Typey.Type where


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
