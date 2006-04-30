
module Core.Read(readCore) where

import Core.Type
import Core.Reduce

import List
import Char

readCore :: String -> Core
readCore ('=':xs) =  readCore $ tail $ dropWhile (/= '\n') xs
readCore xs = reduce $ moduleNaming $ removeTup $ readSpecial xs


-- A special version of Read Core
-- While only operates on Yhc generated Core
-- But can be more lazy
readSpecial :: String -> Core
readSpecial xs = read xs -- Core $ concatMap f (lines xs)
    where
        f "Core []" = []
        f "Core" = []
        f "]" = []
        f "" = []
        f x = [read $ tail $ dropWhile isSpace x]


-- efficiency hack!
removeTup :: Core -> Core
removeTup (Core n c) = Core n $ filter f c
    where
        f (CoreFunc (CoreApp (CoreVar x) _) _) | "Preamble.tup" `isPrefixOf` x = False
        f _ = True
        

-- fix LAMBDA to be Module.LAMBDA
-- add Module. to position information
moduleNaming :: Core -> Core
moduleNaming (Core modName c) = Core modName $ mapCore f c
    where
        f (CoreVar x) | "LAMBDA" `isPrefixOf` x = CoreVar (modName ++ "." ++ x)
        f (CorePos p x) = CorePos (modName ++ p) x
        f x = x
