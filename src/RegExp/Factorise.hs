
module RegExp.Factorise(factorise) where

import RegExp.Type
import Maybe
import RegExp.Parse



factorise :: (Show a, Eq a) => RegExp a -> RegExp a
factorise x = mapRegExp f x
    where
        f (RegUnion xs) = regUnion $ factorList $ factorList $ factorList xs
        f x = x


factorList :: (Show a, Eq a) => [RegExp a] -> [RegExp a]
factorList x = applyFunc f x
    where
        f a b = factorPair (g a) (g b)
        
        g (RegConcat x) = x
        g x = [x]



factorPair :: (Show a, Eq a) => [RegExp a] -> [RegExp a] -> Maybe (RegExp a)
factorPair xs ys = if mx == 0 then Nothing else Just combine
    where
        mx = maximum (map fst res)
        (x2,y2) = snd $ head $ filter ((==) mx . fst) res
    
        res = [(matchLen (drop x xs) (drop y ys),(x,y)) |
            x <- [0 .. length xs - 1],
            y <- [0 .. length xs - 1]]
        
        combine = regConcat [
                        f (take x2 xs) (take y2 ys),
                        regConcat $ take mx $ drop x2 xs,
                        f (drop (x2+mx) xs) (drop (y2+mx) ys)
                    ]
        
        f [] [] = regLambda
        f [] ys = regUnion [regLambda, regConcat ys]
        f xs [] = regUnion [regLambda, regConcat xs]
        f xs ys = regUnion [regConcat xs, regConcat ys]


matchLen :: (Show a, Eq a) => [RegExp a] -> [RegExp a] -> Int
matchLen (a:as) (b:bs) | a == b = 1 + matchLen as bs
matchLen _ _ = 0


-- stolen from pred :)
-- pita because of Eq
applyFunc :: (Show a, Eq a) => (RegExp a -> RegExp a -> Maybe (RegExp a)) -> [RegExp a] -> [RegExp a]
applyFunc f [] = []
applyFunc f (x:xs) =
        if all isNothing res then
            x : applyFunc f xs
        else
            g (zip res xs)
    where
        res = map (f x) xs
        
        g ((Nothing,y):ys) = y : g ys
        g ((Just a ,y):ys) = a : map snd ys
        g _ = error "applyFunc failed, logic error"
