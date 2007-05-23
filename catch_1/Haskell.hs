
module Haskell(Partial, parseHaskell) where

import Data.Char
import Data.List

-- name, possible precondition
type Partial = (String,String)


parseHaskell :: String -> ([String], [Partial])
parseHaskell s = (parseModule s, parsePragmas s)


parseModule s = case lexList $ dropComments s of
                    ("module":xs) -> f xs
                    _ -> []
    where
        f ("where":_) = []
        f (x:xs) = [x | isLower (head x)] ++ f xs
        f _ = []


lexList x = case lex x of
                [("","")] -> []
                [(x,xs)] -> x : lexList xs
                _ -> []


dropComments ('-':'-':xs) = dropComments $ dropWhile (/= '\n') xs
dropComments (x:xs) | isSpace x = dropComments xs
dropComments ('{':'-':xs) = f xs
    where
        f ('-':'}':xs) = dropComments xs
        f (x:xs) = dropComments xs
        f [] = []
dropComments x = x


parsePragmas = concatMap readPartial . tails

readPartial ('{':'-':'#':xs)
        | "PARTIAL" `isPrefixOf` xs2 && not (null ys2) && isSpace (head ys2)
        = parsePartial $ filter (/= ",") $ lexList ys2
    where
        ys2 = drop 7 xs2
        xs2 = dropWhile isSpace xs
readPartial _ = []

parsePartial (('#':_):_) = []
parsePartial (name:s@('\"':_):rest) = (name,read s) : parsePartial rest
parsePartial (name:rest) = (name,"") : parsePartial rest
parsePartial _ = []

