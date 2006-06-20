
module Typey.CtorTypes(ctorTypes) where

import Typey.Type
import Typey.Subtype
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char


-- get all the basic type information for constructors
ctorTypes :: DataM SmallT -> TypeList
ctorTypes datam = concatMap (dataSubtypes . snd) datam


dataSubtypes :: DataT SmallT -> TypeList
dataSubtypes (DataT n xs) = map f xs
    where
        getSelfs :: Char -> [TSubtype]
        getSelfs i = [TBind [TPair [a] (lst '1')] | a <- nas] ++
                     [TBind [TPair [a] (lst '1'), TPair [b] (lst '2')] | a <- ras, b <- nas ++ ras]
            where
                (ras,nas) = (\(a,b) -> (map snd a,map snd b)) $ partition fst
                    [(isRecursive ctr, q) | ctr@(CtorT q _) <- xs]
                
                lst j = take n [TFree [[c,i,j]] | c <- ['a'..'z']]
    
        f (CtorT name xs) = (,) name [TArr a (makeRes name (zip xs a)) | a <- args]
            where args = crossProduct $ zipWith g ['a'..] xs
        
        g i Self = getSelfs i
        g i (FreeS n) = [TFree [[i]]]

        makeRes :: String -> [(SmallT,TSubtype)] -> TSubtype
        makeRes name xs | not $ any (isSelf.fst) xs = TBind [TPair [name] vars]
                        | otherwise = TBind [TPair [name] vars, selfs]
            where
                selfs = unionListNote "dataSubtypes.selfs" [y | (Self,TBind x) <- xs, y <- x]
                vars = map h [0..n-1]
                h i = unionList (TFree [] : [x | (FreeS j, x) <- xs, j == i])

