-- !!! Wentworth's version of a program to generate
-- !!! all the expansions of a generalised regular expression
-- !!!
--
module Gen_Regexps_Safe where

-- CHANGE: was import Char (needs fixing, but later)
import Data.Char

main = interact (("Enter a generator: " ++).show.expand.takeWhile (/= '\n'))

expand []	= [""]
expand ('<':x)	= numericRule x
expand ('[':x)	= alphabeticRule x
expand x	= constantRule x

constantRule (c:rest) = [ c:z | z <- expand rest ]

alphabeticRule (a:'-':b:']':rest)
  | a <= b  	= [c:z | c <- [a..b],	      z <- expand rest]
  | otherwise	= [c:z | c <- reverse [b..a], z <- expand rest]
alphabeticRule _ = []

numericRule x
  = [ pad (show i) ++ z
	| i <- if u < v then [u..v] else [u,u-1..v]
	, z <- expand s ]
  where
    (p,q) = safeSpan (/= '-') x
    (r,s) = safeSpan (/= '>') q
    (u,v)   = (mknum p, mknum r)
    mknum s = foldl (\ u c -> u * 10 + (ord c - ord '0')) 0 s
    pad s   = [ '0' | i <- [1 .. (width-(length s))]] ++ s
    width   = max (length (show u)) (length (show v))


safeSpan p x = (a, safeTail x)
    where (a,b) = span p x

safeTail (x:xs) = xs
safeTail x = x
