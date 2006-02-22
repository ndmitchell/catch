
module Core.Reduce(reduce) where

import Core.Type
import List
import Maybe
import General.TextUtil
import Numeric
import Char


reduce (Core x) = Core $ map f x

f (CoreFunc a b) = CoreFunc (mapCore g a) (mapCore g b)
    where
        g (CoreCon x) = CoreCon (redName x)
        g (CoreVar x) = CoreVar (redName x)
        g x = x

f (CoreData n x) = CoreData (redName n) (map g x)
    where
        g (CoreCtor n xs) = CoreCtor (redName n) (map h xs)
        
        h Nothing = Nothing
        h (Just x) = Just (redName x)


redName x = concat $ intersperse "." $ mapMaybe g $ splitList "." x
    where
        g "Preamble" = Nothing
        g "YHC" = Nothing
        g "Internal" = Nothing -- only valid if YHC is first
        g "Prelude" = Nothing
        g "catch_any" = Just "_"
        g x | "Preamble_Hex_" `isPrefixOf` x = Just $ makeHexStr $ drop 13 x
        g x | "Preamble_" `isPrefixOf` x = Just $ drop 9 x
        g "()" = Just "Tup0"
        g "1()" = Just "Tup1"
        g x | all isDigit x = Just $ "Tup" ++ x
        g ('(':xs) | last xs == ')' && all (== ',') (init xs) = Just $ "Tup" ++ show (length xs)
        g "_" | x /= "_" = Just "Default" -- _ in a module name is a default class method
        g x = Just x

        makeHexStr [] = []
        makeHexStr (a:b:c) = makeHexChr a b : makeHexStr c
        
        makeHexChr a b = chr x
            where [(x,"")] = readHex [a,b]
