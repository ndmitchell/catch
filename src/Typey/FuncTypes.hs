
module Typey.FuncTypes(funcTypes) where

import Typey.Type
import Typey.Subtype
import Hite
import General.General
import Data.Maybe
import Data.List
import Data.Char


funcTypes :: DataM SmallT -> Func2M -> TypeList
funcTypes datam funcm = [(name, getFuncSubtypes datam typ) | (name, typ) <- funcm]

-- get all the possible subtypes of a function
getFuncSubtypes :: DataM SmallT -> Large2T -> [([TSubtype],TSubtype)]
getFuncSubtypes datam (Arr2T a b) = [(uniqueFrees x,TFree []) | x <- getSubtypesList datam a]
getFuncSubtypes datam x = [([], TFree [])]

{-

= renameVars . {- addBottoms . -} getSubtypes datam


getSubtypes :: DataM SmallT -> Large2T -> [([TSubtype], [TSubtype])]
getSubtypes datam (Free2T a) = ([],[TFree [a]])
getSubtypes datam (Arr2T a b) = [(x,getSubtypes datam b) | x <- as]
    where as = crossProduct $ map (getSubtypes datam) a
getSubtypes datam (Ctor2T a) = getSubtypes datam $ Bind2T (Ctor2T a) []

getSubtypes datam (Bind2T (Ctor2T x) xs) = map (simpSubtype datat) $
        [TBind [TPair [a] b] | a <- nas, b <- bs] ++
        [TBind [TPair [a1] b1, TPair [a2] b2] | a1 <- ras, b1 <- bs, a2 <- ras ++ nas, b2 <- bs]
    where
        datat = fromJust $ lookup x datam
        (ras,nas) = (\(a,b) -> (map snd a,map snd b)) $ partition fst
            [(isRecursive ctr, q) | let DataT _ y = datat, ctr@(CtorT q _) <- y]
        bs = crossProduct $ map (getSubtypes datam) xs


simpSubtype :: DataT SmallT -> TSubtype -> TSubtype
simpSubtype (DataT _ ctors) (TBind xs) = TBind (map f xs)
    where
        f (TPair [n] xs) = TPair [n] (zipWith f [0..] xs)
            where
                ctr = [y | CtorT n2 ys <- ctors, n2 == n, FreeS y <- ys]
                f i x = if i `elem` ctr then x else TFree []



-- add bottom everywhere you can
addBottoms :: [TSubtype] -> [TSubtype]
addBottoms x = concat $ map f x
    where
        f (TArr as bs) = concat [[TArr a b, TArr a TBot] | a <- fs as, b <- f bs]
        f (TBind xs) = map TBind $ crossProduct $ map g xs
        f x = [x]
        
        fs xs = crossProduct $ map f xs
        
        g (TPair as bs) = [TPair as b | b <- fs bs]


-- rename all variables
renameVars :: [TSubtype] -> [TSubtype]
renameVars x = concatMap uniqueVars x



uniqueVars :: TSubtype -> [TSubtype]
uniqueVars (TArr arg res) = [TArr lhs b | b <- rhss]
    where
        lhs = replaceFrees arg $ f 0 $ extractFrees arg
        lhsf = [x | TFree [x] <- extractFrees lhs]
        
        rhs = extractFrees [res]
        rhsf = [x | TFree [x] <- rhs]
        
        rhss = if null rhsf then [res]
               else concatMap (replaceFrees [res]) $ concatMap g (allItems rhs)
        
        g (pre,TFree [x],post) = [map blank pre ++ [TFree [y]] ++ map blank post | y <- h x]
        g _ = []
        
        blank (TFree [x]) = TFree []
        blank x = x

        h x = [y | y <- lhsf, takeWhile isAlpha y == x]
    
        f n (TFree [x]:xs) = TFree [x ++ show n] : f (n+1) xs
        f n (x:xs) = x : f n xs
        f n [] = []

-- if there are any free variables, then die, since they have no input
uniqueVars res | null frees = [res]
               | otherwise = []
    where
        frees = [x |TFree [x] <- extractFrees [res]]
-}