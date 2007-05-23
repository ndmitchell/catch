
module Haskell(parseHaskell, Pragmas(..)) where

import Data.Char
import Data.List

data Pragmas = Pragmas {exports :: [String]
                       ,partial :: [String]
                       ,regress :: String
                       } deriving Show

parseHaskell :: String -> Pragmas
parseHaskell s = Pragmas (parseExports s) (parsePartial s) (parseRegress s)


parseExports s =
    case lexList s of
        ("module":xs) -> f xs
        _ -> []
    where
        f ("where":_) = []
        f (x:xs) = [x | isLower (head x)] ++ f xs
        f _ = []


lexList x = case lex x of
                [("","")] -> []
                [('-':'-':_,xs)] -> lexList $ dropWhile (/= '\n') xs
                [("{",'-':xs)] -> f xs
                [(x,xs)] -> x : lexList xs
                _ -> []
    where
        f ('-':'}':xs) = lexList xs
        f (x:xs) = f xs
        f [] = []


parsePartial = concatMap (filter f . lexList) . parsePragmas "PARTIAL"
    where
        f (x:xs) | isLower x = True
        f "," = False
        f x = error $ "PARTIAL pragma parse error: " ++ x


parseRegress = f . concatMap lexList . parsePragmas "CATCH"
    where
        f [xs@('\"':_)] = read xs :: String
        f [] = []
        f x = error $ "CATCH pragma parse error: " ++ show x


parsePragmas :: String -> String -> [String]
parsePragmas name = concatMap f . tails
    where
        f ('{':'-':'#':xs)
                | name `isPrefixOf` xs2 && not (null ys2) && isSpace (head ys2)
                = [g ys2]
            where
                ys2 = drop (length name) xs2
                xs2 = dropWhile isSpace xs
        f _ = []

        g ('#':'-':'}':_) = []
        g (x:xs) = x : g xs
        g _ = []
