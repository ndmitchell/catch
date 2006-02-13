
module Hite.Data where

import Hite.Type
import General.TextUtil
import List
import Char
import General.General


readData :: String -> [Data]
readData x = map f $ join $ map (:[]) $ filter (not . isBlank) $ lines x
    where
        isBlank xs = null pre || "--" `isPrefixOf` pre
            where pre = trimLeft xs
        
        join :: [[String]] -> [[String]]
        join (a:[b]:c) | isSpace (headNote "readData" b) = join ((a ++ [b]) :c)
                       | otherwise = a : join ([b]:c)
        join [x] = [x]
        join []  = []
        
        f (name:alts) = Data (trim name) (map (g . oneSpace . trim) alts)
        
        oneSpace (' ':' ':xs) = oneSpace (' ':xs)
        oneSpace (x:xs) = x : oneSpace xs
        oneSpace [] = []
        
        g x = Ctor name args
            where (name:args) = splitList " " x


-- instead of using the :@1 notation, use hd
-- also remove _ from LHS of case matches
-- as dictated by the data
fixData :: Hite -> Hite
fixData h = mapExpr f h
    where
        f (Sel x arg) | '$' `elem` arg = Sel x (ctorArgs (getCtor a h) !! read b)
            where (a,_:b) = break (== '$') arg
        
        f (Case x alts) = Case x (concatMap g alts)
            where
                allCtors = map ctorName $ ctors $ getDataFromCtor (headNote "Hite.Data.fixData" myCtors) h
                myCtors = filter (/= "_") $ map fst alts
                
                g ("_", b) = zip (allCtors \\ myCtors) (repeat b)
                g (a  , b) = [(a,b)]
    
        f x = x
