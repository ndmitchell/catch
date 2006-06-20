
module Typey.CtorTypes(ctorTypes,typePermutations) where

import Typey.Type
import Typey.Subtype
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char


-- get all the basic type information for constructors
ctorTypes :: DataM SmallT -> TypeList
ctorTypes datam = concatMap (dataSubtypes . snd) $ tail datam


dataSubtypes :: DataT SmallT -> TypeList
dataSubtypes dat@(DataT n xs) = map f xs
    where
        f (CtorT name xs) = (,) name [TArr a (makeRes name (zip xs a)) | a <- args]
            where args = map uniqueFrees $ crossProduct $ map g xs
        
        g Self = typePermutations dat
        g (FreeS i) = [TFree [[numToChr i]]]
        
        makeRes :: String -> [(SmallT,TSubtype)] -> TSubtype
        makeRes name xs | not $ any (isSelf.fst) xs = TBind [TPair [name] vars]
                        | otherwise = TBind [TPair [name] vars, selfs]
            where
                selfs = unionListNote "dataSubtypes.selfs" [y | (Self,TBind x) <- xs, y <- x]
                vars = map h [0..n-1]
                h i = unionList (TFree [] : [x | (FreeS j, x) <- xs, j == i])



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
