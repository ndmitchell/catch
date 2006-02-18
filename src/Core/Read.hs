
module Core.Read(readCore) where

import Core.Type
import Core.Reduce

import List
import Char

readCore :: String -> Core
readCore ('=':xs) =  readCore $ tail $ dropWhile (/= '\n') xs
readCore xs = reduce $ lambdaFix $ removeTup $ readSpecial xs


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
removeTup (Core c) = Core $ filter f c
    where
        f (CoreFunc (CoreApp (CoreVar x) _) _) | "Preamble.tup" `isPrefixOf` x = False
        f _ = True
        

-- fix LAMBDA to be Module.LAMBDA
lambdaFix :: Core -> Core
lambdaFix (Core c) = Core $ mapCore f c
    where
        modName = reverse $ dropWhile (/= '.') $ reverse $ head
            [x | CoreFunc (CoreApp (CoreVar x) _) _ <- c, '.' `elem` x]
        
        f (CoreVar x) | "LAMBDA" `isPrefixOf` x = CoreVar (modName ++ x)
        f x = x
