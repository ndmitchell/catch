
module Typey.Show where

import Typey.Type
import Data.List


instance Show FuncT where
    show (FuncT n args res) = "forall " ++ show n ++ " . " ++ (concat $ intersperse " -> " $ map show $ args ++ [res])
    
instance Show LargeT where
    show (FreeL i) = "#" ++ show i
    show (CtorL name args) = "(" ++ name ++ concatMap ((' ':) . show) args ++ ")"

