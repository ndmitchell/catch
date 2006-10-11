
module Convert.CoreData(convData) where

import Core
import Hite

import Data.List
import Data.Char


convData :: CoreData -> Data
convData (CoreData dname typ ctors) = Data dname (g [] ctors) typ
    where
        g seen [] = []
        g seen (x:xs) = res : g (args++seen) xs
            where
                res@(Ctor _ args _) = f seen x
    
        f seen (CoreCtor cname args) = Ctor cname (zipWith g [1..] $ map snd args) (map (convType . fst) args)
            where
                g n (Just x) = demandUnique seen x
                g n (Nothing) | cname == "Prelude.:" = ["","hd","tl"] !! n
                              | cname == "Prelude.1()" = "tup1_1"
                              | "Prelude.(" `isPrefixOf` cname =
                                        let i = length (filter (==',') cname) + 1
                                        in "tup" ++ show i ++ "_" ++ show n
                              | otherwise = (map toLower $ reverse $ takeWhile (/= '.') $ reverse cname) ++ "_" ++ show n

        demandUnique seen x | x `notElem` seen = x
                            | otherwise = head [x2 | i <- [1..], let x2 = x ++ show i, x2 `notElem` seen]


-- hacky, will work a bit
convType :: String -> TyType
convType "" = TyNone
convType ('(':xs) = convType (init xs)
convType x = f (words x)
    where
        f [x] | isLower (head x) = TyFree x
        f (x:xs) = TyCon (g x) (map TyFree xs)
        
        g x | "Prelude."  `isPrefixOf` x = drop 8 x
            | "Preamble." `isPrefixOf` x = drop 9 x
            | otherwise = x

