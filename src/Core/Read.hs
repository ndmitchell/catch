
module Core.Read(readCore) where

import Core.Type
import Core.Reduce

readCore :: String -> Core
readCore ('=':xs) = reduce $ read (lines xs !! 1)
readCore xs = reduce $ read xs
