
module Core.Read(readCore) where

import Core.Type
import Core.Reduce

import List

readCore :: String -> Core
readCore ('=':xs) =  readCore (lines xs !! 1)
readCore xs = reduce $ lambdaFix $ read xs



-- fix LAMBDA to be Module.LAMBDA
lambdaFix :: Core -> Core
lambdaFix (Core c) = Core $ mapCore f c
    where
        modName = reverse $ dropWhile (/= '.') $ reverse $ head
            [x | CoreFunc (CoreApp (CoreVar x) _) _ <- c, '.' `elem` x]
        
        f (CoreVar x) | "LAMBDA" `isPrefixOf` x = CoreVar (modName ++ x)
        f x = x
