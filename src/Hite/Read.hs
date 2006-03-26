

module Hite.Read() where

import Hite.Type
import Hite.ReadExpr

import General.TextUtil
import Char
import List



instance Read Hite where
    readsPrec n x = [(readHite x, "")]
    

readHite :: String -> Hite
readHite x = Hite (map readData datas) (map readFunc funcs)
    where (datas, funcs) = partition ("data " `isPrefixOf`) $
                           groupIndent $ filter (not . all isSpace) $ lines x



groupIndent :: [String] -> [String]
groupIndent (x:y:xs) | null y = groupIndent (x:xs)
                     | isSpace (head y) = groupIndent ((x ++ "\n" ++ y) : xs)
                     | otherwise = x : groupIndent (y:xs)
groupIndent x = x


singleSpace :: String -> String
singleSpace (' ':' ':xs) = singleSpace (' ':xs)
singleSpace (x:xs) = x : singleSpace xs
singleSpace [] = []


readData :: String -> Data
readData x = Data (trim name) (map readCtor $ splitList "|" ctors)
    where (name, '=':ctors) = break (== '=') $ drop 5 x

readCtor x = Ctor name args
    where (name:args) = splitList " " $ singleSpace $ trim x


readFunc :: String -> Func
readFunc x = Func name args (readExpr args $ trimLeft expr)
    where
        (name:args) = splitList " " $ singleSpace $ trim prefix
        (prefix, '=':expr) = break (== '=') x


