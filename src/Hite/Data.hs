
module Hite.Data where

import Hite.Type
import TextUtil
import List
import Char


readData :: String -> [Data]
readData x = map f $ join $ map (:[]) $ filter (not . isBlank) $ lines x
    where
        isBlank xs = null pre || "--" `isPrefixOf` pre
            where pre = trimLeft xs
        
        join :: [[String]] -> [[String]]
        join (a:[b]:c) | isSpace (head b) = join ((a ++ [b]) :c)
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
fixData x = x

{-

                g (CoreVar "_", x) = map (\a -> (a, body)) (allCtors \\ ctorNames)
                    where
                        body = f subs x
                        
                        ctorNames = concatMap (h . fst) alts
                        h (CoreApp (CoreCon x) _) = [x]
                        h _ = []
                    
                        allCtors = map ctorName $ ctors $
                            getDataFromCtor (head ctorNames) (Hite types [])
                        
-}