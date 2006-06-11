
module Typey.Type where

import Hite
import General.General
import Data.Maybe


-- TYPE STUFF

type FuncM = [(FuncName, FuncT)]
type DataM a = [(DataName, DataT a)]

data FuncT = FuncT Int [LargeT] LargeT
data DataT a = DataT Int [CtorT a]
data CtorT a = CtorT String [a]
data LargeT = FreeL Int | CtorL String [LargeT]
data SmallT = FreeS Int | Self

isRecursive (CtorT name xs) = any isSelf xs
isSelf (Self) = True; isSelf _ = False


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



getSubtypesData :: DataT SmallT -> [Subtype]
getSubtypesData (DataT n ctors) = concatMap f ctors
    where
        f c@(CtorT name _) | not (isRecursive c) = [Subtype [UCtor name] [] [] []]
                           | otherwise = [Subtype [UCtor name] [UCtor x] [] [] | CtorT x _ <- ctors]

getSubtypesLarge :: DataM SmallT -> LargeT -> [Subtype]
getSubtypesLarge datam (FreeL n) = [Top]
getSubtypesLarge datam (CtorL name args) = concatMap f datat
    where
        f (Subtype a b _ _) | null b = [Subtype a [] x [] | x <- children]
                            | otherwise = [Subtype a b x y | x <- children, y <- children]
    
        datat = getSubtypesData $ fromJust $ lookup name datam
        children = crossProduct $ map (getSubtypesLarge datam) args

getSubtypesFunc :: DataM SmallT -> FuncT -> [[Subtype]]
getSubtypesFunc datam (FuncT n args res) = crossProduct $ map (getSubtypesLarge datam) args

