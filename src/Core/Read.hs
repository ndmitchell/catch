
module Core.Read(readCore) where

import Core.Type
import Core.Reduce
import General.General

import List
import Char

readCore :: String -> Core
readCore ('-':xs) =  readCore $ tail $ dropWhile (/= '\n') xs
readCore xs = reduce $ moduleNaming $ removeTup $ readSpecial xs


-- A special version of Read Core
-- While only operates on Yhc generated Core
-- But can be more lazy
readSpecial :: String -> Core
readSpecial xs = readNote "Core.readSpecial" xs -- Core $ concatMap f (lines xs)
    where
        f "Core []" = []
        f "Core" = []
        f "]" = []
        f "" = []
        f x = [readNote "Core.readSpecial.f" $ tail $ dropWhile isSpace x]


-- efficiency hack!
removeTup :: Core -> Core
removeTup (Core n deps c) = Core n deps $ filter f c
    where
        f (CoreFunc (CoreApp (CoreVar x) _) _) | "tup" `isPrefixOf` x = False
        f _ = True
        

-- add Module. to position information
moduleNaming :: Core -> Core
moduleNaming (Core modName deps c) = Core modName deps $ mapCore f c
    where
        f (CorePos p x) = CorePos (modName ++ p) x
        f x = x
