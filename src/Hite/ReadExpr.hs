
module Hite.ReadExpr(readExpr) where

import Hite.Type
import TextUtil
import Char
import List


isKeywordChar x = isAlphaNum x


readExpr :: String -> Expr
readExpr x = noAfter $ expr x
    where
        noAfter (x, "") = x
    
        rExprs :: String -> (Expr, String)
        rExprs x = let (a:b) = f (trimLeft x) in (Call a b, "")
            where
                f "" = ([], "")
                f ('o':'f':x:xs) | isSpace x = ([], x:xs)
                f (')':xs) = ([], xs)
                f xs = (e:es, done)
                    where
                        (e , todo) = rExpr xs
                        (es, done) = f (trimLeft todo)
        
    
        rExpr :: String -> (Expr, String)
        rExpr x = if "case " `isPrefixOf` x then rCase x
                  else if "(" `isPrefixOf` x then rBrack x
                  else rName x
        
        rName x = (Var name path, b)
            where
                (name:path) = splitList "." a
                (a,b) = break (not . isKeywordChar) x
        
        rBrack x = rExprs x
        
        rCase x = (Case pre alts, rest)
            where
                (pre, body) = rExprs 
                (alts, rest) = rAlts body
                
        rAlts :: String -> ([(String, [Expr])], String)
        rAlts x = if "\n" `isPrefixOf` tx
                  then (let (a,b) = rLines (tail tx) in (map (noAfter . rAlt) a, b))
                  else (let (a,b) = rAlt tx in ([a], b))
            where
                tx = dropWhile (== ' ') x
                (a,b) = rLines (tail tx)
        
        rAlt :: String -> ((String, Expr), String)
        rAlt x = ((trim a, c), d)
            where
                (a,b) = splitPair "->" x
                (c,d) = readExpr b
