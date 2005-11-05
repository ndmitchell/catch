
module Hite.ReadExpr(readExpr) where

import Hite.Type
import TextUtil
import Char
import List


isKeywordChar x = isAlphaNum x || x == '_'


readExpr :: [FuncArg] -> String -> Expr
readExpr args x = rewrite $ noAfter $ rExprs (filter (/= '\r') x)
    where
        -- rewrite a function into standard form
        rewrite (Call (Var (x:xs) []) args) | isUpper x = Make (x:xs) (map rewrite args)
        rewrite (Var (x:xs) []) | isUpper x = Make (x:xs) []
        
        rewrite (Call x []) = rewrite x
        rewrite (Call x args) = Call (rewrite x) (map rewrite args)
        rewrite (Case x alts) = Case (rewrite x) $ map (\(a,b) -> (a, rewrite b)) alts
        rewrite (Var x []) | not (x `elem` args) = CallFunc x
        rewrite x = x
    
    
        noAfter (x, "") = x
    
        rExprs :: String -> (Expr, String)
        rExprs x = let (a:b, c) = f (trimLeft x) in (Call a b, c)
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
        
        rName x = if null a
                  then error $ "Expected name, found: " ++ take 10 x
                  else (Var name path, b)
            where
                (name:path) = splitList "." a
                (a,b) = break (\x -> not (isKeywordChar x || x == '.')) x
        
        rBrack x = rExprs (tail x)
        
        rCase x = (Case pre alts, rest)
            where
                (pre, body) = rExprs $ drop 5 x
                (alts, rest) = rAlts body
                
        rAlts :: String -> ([(String, Expr)], String)
        rAlts x = if "\n" `isPrefixOf` tx
                  then (let (a,b) = rLines (tail tx) in (map (noAfter . rAlt) a, b))
                  else (let (a,b) = rAlt tx in ([a], b))
            where
                tx = dropWhile (== ' ') x
                (a,b) = rLines (tail tx)
        
        rAlt :: String -> ((String, Expr), String)
        rAlt x = ((trim a, c), d)
            where
                Just (a,b) = splitPair "->" x
                (c,d) = rExprs b
                
        
        rLines :: String -> ([String], String)
        rLines x = f [] (lines x)
            where
                deep = depth x
                depth x = length $ takeWhile isSpace x
                
                f acc [] = (reverse acc, "")
                f acc (x:xs) | deep == dx = f (x:acc) xs
                             | deep <  dx = f ((x++head acc):tail acc) xs
                             | deep >  dx = (reverse acc, unlines (x:xs))
                    where dx = depth x
