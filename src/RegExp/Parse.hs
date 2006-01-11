{- |
    Module for converting between regular expressions and text strings
    
    The standard theoretical notation is used, with (*), (+) and (.) as the operators.
    The (.) may be ommitted. The (*) is a postfix operator.
    
-}

module RegExp.Parse(showBy, readBy, readRegExpChar, readByRaw, readRegExpCharRaw) where

import RegExp.Type

import List
import RegExp.General

---------------------------------------------------------------------
-- SHOW SUPPORT

instance (Eq a, Show a) => Show (RegExp a) where
    show x = showBy showNoQuote x


data Order =  OUnion | OConcat | OKleene
    deriving (Eq, Ord)
    

-- | Show a regular expression, using a particular output for each atom.
--   The standard 'show' is defined as 'showBy' 'showNoChar'
showBy :: (Eq a, Show a) => (a -> String) -> RegExp a -> String
showBy showme x = f OUnion x
    where
        f p (RegLit           x) = showme x
        f p (RegKleene RegOmega) = textLambda
        f p (RegOmega          ) = textOmega

        f p (RegKleene x) = f OKleene x ++ "*"
        f p (RegConcat x) = brack (OConcat < p) $ join OConcat "."  x
        f p (RegUnion  x) = brack (OUnion  < p) $ join OUnion  "+" x

        brack False s = s
        brack True  s = "(" ++ s ++ ")"

        join p mid xs = concat $ intersperse mid $ map (f p) xs


---------------------------------------------------------------------
-- READ SUPPORT


-- | Read a string, convert to a regular expression.
--   The main input mechanism
readRegExpChar :: String -> RegExpChar
readRegExpChar = reduceRegExp . readRegExpCharRaw


-- | Used to create 'readRegExpChar'
readBy :: (String -> [Char]) -> String -> RegExp Char
readBy f = reduceRegExp . readByRaw f


-- | Read but do not perform reduction to lowest form
readRegExpCharRaw :: String -> RegExp Char 
readRegExpCharRaw = readByRaw id

-- | Doesn't work unless a is a char.
--   Currently pretty broken...
readByRaw :: (String -> [Char]) -> String -> RegExp Char
readByRaw f x = (deuni . destar . pregex . filter (/= '\'')) x
    where
        destar (x:RegLit '*':xs) = destar (RegKleene x:xs)
        destar (x:xs)     = x : destar xs
        destar []         = []
        
        deuni xs = RegUnion (map RegConcat (divideOn isLitPlus xs))
        
        isLitPlus (RegLit '+') = True; isLitPlus _ = False


pregex :: String -> [RegExpChar]
pregex xs@('(':_) = readRegExpCharRaw inside : pregex rest
    where (inside, rest) = bracket ("(",")") xs

pregex ('^':xs) = regLambda : pregex xs
pregex ('0':xs) = regOmega  : pregex xs
pregex (x  :xs) = RegLit x  : pregex xs
pregex [      ] = []


-- bracket ("(", ")") "(t(e)st) other" = ("t(e)st"," other")
bracket :: (String, String) -> String -> (String, String)

bracket (open, shut) s = (take (n-1) (tail s), drop (n+1) s)
    where
        n = f 0 [] s
        
        f n stack (x:xs) | x `elem` open = f (n+1) (shutOf x : stack) xs
        f n (s:stack) (x:xs) | s == x    = if null stack then n else f (n+1) stack xs
        f n stack (x:xs) = f (n+1) stack xs
        f n stack [] = n
        
        shutOf x = endOf open shut x
        endOf (o:os) (s:ss) x = if o == x then s else endOf os ss x


divideOn :: (a -> Bool) -> [a] -> [[a]]
divideOn f [] = [[]]
divideOn f (x:xs) | f x = [] : divideOn f xs
                  | otherwise = ((x:r):res)
                      where (r:res) = divideOn f xs


