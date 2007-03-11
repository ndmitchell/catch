
module General.General(module General.General, trace) where

import Data.List
import Debug.Trace


type Result a = (Bool,a)

success = (,) True
failure = (,) False


snub :: Ord a => [a] -> [a]
snub = map head . group . sort 
