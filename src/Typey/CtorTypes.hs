
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
ctorTypes datam = concatMap (dataSubtypes datam) datam


dataSubtypes :: DataM SmallT -> (String, DataT SmallT) -> TypeList
dataSubtypes datam (datName, dat@(DataT n xs)) = map f xs
    where
        f (CtorT name xs) = (,) name [(a, makeRes name (zip xs a)) | a <- args]
            where args = map uniqueFrees $ getSubtypesList datam $ smallToLarge n datName xs
        
        makeRes :: String -> [(SmallT,TSubtype)] -> TSubtype
        makeRes name xs | not $ any (isSelf.fst) xs = TBind [TPair [name] vars]
                        | otherwise = TBind [TPair [name] vars, selfs]
            where
                selfs = unionListNote "dataSubtypes.selfs" [y | (Self,TBind x) <- xs, y <- x]
                vars = map h [0..n-1]
                h i = unionList (TFree [] : [x | (FreeS j, x) <- xs, j == i])


smallToLarge :: Int -> CtorName -> [SmallT] -> [Large2T]
smallToLarge n ctr xs = map f xs
    where
        f Self = Bind2T (Ctor2T ctr) [Free2T [numToChr i] | i <- [0..n-1]]
        f (FreeS i) = Free2T [numToChr i]


{-
typePermutations :: DataT SmallT -> [TSubtype]
typePermutations (DataT n xs) =
        [TBind [x]| x <- nper] ++ [TBind [x,y] | x <- rper, y <- rper++nper]
    where
        (rper,nper) = (map f rctr, map f nctr)
        (rctr,nctr) = partition isRecursive xs

        f (CtorT name xs) = TPair [name] $ map g [0..n-1]
            where
                g i = if i `elem` frees then TFree [[numToChr i]] else TFree []
                frees = nub [i | FreeS i <- xs]
-}
