
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
