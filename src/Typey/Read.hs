
module Typey.Read(readFuncT) where

import Typey.Type
import Data.Char
import Data.List
import Data.Maybe
import General.General


readFuncT :: String -> FuncT
readFuncT x = FuncT (length free) (init res) (last res)
    where
        res = combine $ lift lexs
        lexs = readLexes x
        free = zip (nub $ filter (isLower . head) lexs) [0..]
        
        -- take an item from a string, to a Type
        lift [] = []
        lift ("(":xs) = one (combine (lift a)) : lift b
            where (a,b) = splitBrackets xs
        lift ("->":xs) = FreeL (-1) : lift xs
        lift (x:xs) | isLower (head x) = (FreeL $ lookupNote "readFuncT" x free) : lift xs
                    | otherwise = CtorL x [] : lift xs

        one [x] = x
        one _ = error "readFuncT, one"

        -- combine a list of LargeT to a singe list separated by Free -1 (or -> arrow)
        combine (x:FreeL (-1):xs) = x : combine xs
        combine (CtorL x ys:z:zs) = combine (CtorL x (ys ++ [z]) : zs)
        combine [x] = [x]
        

splitBrackets :: [String] -> ([String], [String])
splitBrackets xs = f 0 [] xs
    where
        f 0 acc (")":xs) = (reverse acc, xs)
        f n acc (x:xs) = f (n2+n) (x:acc) xs
            where n2 = if x == ")" then -1 else if x == "(" then 1 else 0
        


readLexes :: String -> [String]
readLexes xs = if null a then [] else a : readLexes b
    where [(a,b)] = lex xs
