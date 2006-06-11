
module Typey.Show where

import Typey.Type
import Data.List


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

instance Show UCtor where
    show (UCtor x) = x
    show (UVar n) = "#" ++ show n


