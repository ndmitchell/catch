
module General.General where

import Data.List


type Result a = (Bool,a)

success = (,) True
failure = (,) False


snub :: Ord a => [a] -> [a]
snub = map head . group . sort 

