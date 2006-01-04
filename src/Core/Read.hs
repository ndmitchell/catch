
module Core.Read(readCore) where

import Core.Type
import Core.Reduce

import List

readCore :: String -> Core
readCore ('=':xs) =  reduce $ lambdaFix $ read (lines xs !! 1)
readCore xs = reduce $ lambdaFix $ read xs



-- fix LAMBDA to be Module.LAMBDA
lambdaFix :: Core -> Core
lambdaFix (Core c) = Core $ map f c
    where
        modName = reverse $ dropWhile (/= '.') $ reverse $ head
            [x | CoreFunc (CoreApp (CoreVar x) _) _ <- c, '.' `elem` x]
        
        f (CoreFunc a b) = CoreFunc (g a) (g b)
        
        g (CoreVar x) | "LAMBDA" `isPrefixOf` x = CoreVar (modName ++ x)
        g (CoreApp x y) = CoreApp (g x) (map g y)
        g (CoreCase a bs) = CoreCase (g a) (map (\(a,b) -> (g a, g b)) bs)
        g (CoreLet a b) = CoreLet (map f a) (g b)
        g (CorePos a b) = CorePos a (g b)
        g x = x
