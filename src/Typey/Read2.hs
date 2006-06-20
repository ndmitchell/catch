
module Typey.Read2(readFunc2T) where

import Typey.Type
import Data.Char
import Data.List
import Data.Maybe
import General.General
import Text.ParserCombinators.Parsec


readFunc2T :: String -> Large2T
readFunc2T x = res
    where   
        x2 = dropSpaces x
        res = case (parse readStr "" x2) of
                  Left err -> error $ "parse error while parsing:\n" ++ show x ++ "\n at " ++ show err
                  Right x  -> x

isSpecial x = x `elem` "->"

dropSpaces (' ':x:xs) | isSpecial x = dropSpaces (x:xs)
dropSpaces (x:' ':xs) | isSpecial x = dropSpaces (x:xs)
dropSpaces (x:xs) = x : dropSpaces xs
dropSpaces [] = []


readStr :: Parser Large2T
readStr = do x <- readType
             eof
             return x

readType :: Parser Large2T
readType = readArr

spaces1 = skipMany1 space

keyword :: Parser String
keyword = many1 (alphaNum <|> char '_')

readKeyword :: Parser Large2T
readKeyword = do x <- keyword
                 return $ if isLower (head x) then Free2T x else Ctor2T x

readCtor :: Parser Large2T
readCtor = do x:xs <- readItem `sepBy1` spaces1
              return $ if null xs then x else Bind2T x xs

readItem :: Parser Large2T
readItem = readBracket <|> readKeyword


readArr :: Parser Large2T
readArr = do spaces
             x <- readCtor
             spaces
             (do string "->"
                 y <- readArr
                 return $ joinArr x y)
               <|>
                 return x

joinArr :: Large2T -> Large2T -> Large2T
joinArr x (Arr2T ys y) = Arr2T (x:ys) y
joinArr x y = Arr2T [x] y


readBracket :: Parser Large2T
readBracket = do char '('
                 x <- readType
                 char ')'
                 return x
