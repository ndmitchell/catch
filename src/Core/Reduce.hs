
module Core.Reduce(reduce) where

import Core.Type
import List
import Maybe
import General.TextUtil
import Numeric
import Char


reduce (Core n d x) = Core n d $ map (f n) x

f name (CoreFunc (CoreApp (CoreVar a) as) b) = CoreFunc (CoreApp a2 (mapCore g as)) (mapCore g b)
    where
        a2 = CoreVar $ redName (if '.' `elem` a && any isKeyChar a then a else name ++ "." ++ a)
        g (CoreCon x) = CoreCon (redName x)
        g (CoreVar x) = CoreVar (redName x)
        g x = x
        
        isKeyChar x = isAlphaNum x || x `elem` "'_"

f name (CoreData n t x) = CoreData dname t (map g x)
    where
        dname = redName $ name ++ "." ++ n
        g (CoreCtor n xs) = CoreCtor (redName $ name ++ "." ++ n) xs


redName "Preamble.." = "o"
redName "Prelude.." = "o"
redName x = concat $ intersperse "." $ mapMaybe g $ splitList "." x
    where
        g "" = error $ "Core.Reduce.redName, " ++ show x
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
