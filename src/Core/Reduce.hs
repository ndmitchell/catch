
module Core.Reduce(reduce) where

import Core.Type
import List

reduce (Core x) = Core $ map f x

f (CoreFunc a b) = CoreFunc a (red b)

red (CoreCon x) = CoreCon (redName x)
red (CoreVar x) = CoreVar (redName x)
red (CoreApp x y) = CoreApp (red x) (map red y)
red (CoreCase x y) = CoreCase (red x) (map (\(a,b) -> (red a, red b)) y)
red (CoreLet x y) = CoreLet (map f x) (red y)
red (CorePos x y) = CorePos x (red y)
red x = x


aliases = [
    ("Prelude.Prelude.Eq.Prelude.", ""),
    ("Prelude.Prelude.Num.Prelude.", ""),
    ("Prelude.",""),
    ("YHC.Internal.","")
    ]

redName x = g aliases
    where
        g ((find,rep):xs) | find `isPrefixOf` x = rep ++ drop (length find) x
                          | otherwise = g xs
        g [] = x
