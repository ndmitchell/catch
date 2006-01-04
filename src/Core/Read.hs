
module Core.Read(readCore) where

import Core.Type

readCore :: String -> Core
readCore ('=':xs) = read (lines xs !! 1)
readCore xs = read xs
