
module Typey.Type where

import Hite
import General.General
import Data.Maybe
import Data.List
import Control.Exception


-- SHOW STUFF

instance Show FuncT where
    show (FuncT n args res) = "forall " ++ show n ++ " . " ++ (concat $ intersperse " -> " $ map show $ args ++ [res])
    
instance Show LargeT where
    show (FreeL i) = "#" ++ show i
    show (CtorL name args) = "(" ++ name ++ concatMap ((' ':) . show) args ++ ")"

instance Show SmallT where
    show (FreeS i) = "#" ++ show i
    show Self = "*"

instance Show a => Show (DataT a) where
    show (DataT n xs) = "forall " ++ show n ++ " . " ++ (concat $ intersperse " | " $ map show xs)

instance Show a => Show (CtorT a) where
    show (CtorT name xs) = name ++ concatMap ((' ':) . show) xs


instance Show Subtype where
    show (Subtype u1 d1 u2 d2) = "Subtype " ++ show u1 ++ " " ++ show d1 ++ " " ++ show u2 ++ " " ++ show d2
    show Top = "?"
    show Bot = "_|_"
    show Empty = "_"
    show (SVar n) = "#" ++ show n

instance Show UCtor where
    show (UCtor x) = x
    show (UVar n) = "#" ++ show n


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
             | Top | Bot | SVar [Int] | Empty
             deriving Eq

data UCtor = UCtor String | UVar Int
             deriving Eq



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



emptySubtype = Subtype [] [] [] []

unionSubtype :: [Subtype] -> Subtype
unionSubtype xs = foldr unionPair Empty xs

unionPair :: Subtype -> Subtype -> Subtype
unionPair Empty x = x
unionPair x Empty = x
unionPair (SVar x) (SVar y) = SVar (x++y)
unionPair Top Top = Top
unionPair s1@(Subtype a1 b1 c1 d1) s2@(Subtype a2 b2 c2 d2) =
        Subtype (f a1 a2) (f b1 b2) (fs c1 c2) (fs d1 d2)
    where
        f x y = x `union` y
        fs [] [] = []
        fs (x:xs) (y:ys) = (x `unionPair` y) : fs xs ys
        fs xs [] = xs
        fs [] ys = ys
unionPair x y = error $ "unionPair: " ++ show (x,y)

-- drop all items to the bottom (recursive) level
collapseSubtype :: Subtype -> Subtype
collapseSubtype (Subtype a b c d) = Subtype [] a [] c `unionPair` Subtype [] b [] d

liftSubtype :: Subtype -> Subtype
liftSubtype (Subtype a b c d) = Subtype b b d d

isSubset :: Subtype -> Subtype -> Bool
isSubset Top Top = True
isSubset Top Empty = False
isSubset (Subtype a1 b1 c1 d1) (Subtype a2 b2 c2 d2) =
        f a1 a2 && f b1 b2 && fs c1 c2 && fs d1 d2
    where
        f x y = null (x \\ y)
        fs x y = and $ zipWith isSubset x y