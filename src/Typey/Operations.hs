
{- |
    These are the mathematical operations on TSubtypes.
    They will all require proof and relate to each other, hence in the same module.
-}

module Typey.Operations(subst, applySubst) where

import Typey.Type
import Typey.Subtype
import General.General
import Data.List
import Data.Maybe
import Control.Monad

-- Compatability of subtypes
-- It's hard to unify two lists of different length

compatTPairs :: [TPair] -> [TPair] -> ([TPair], [TPair])
compatTPairs (x:xs) (y:ys) = (x:f xs, y:f ys)
    where
        hasSecond = not $ null xs && null ys
        f [] | hasSecond = [TPair [] []]
        f x = x


compatTLists :: [TSubtype] -> [TSubtype] -> ([TSubtype],[TSubtype])
compatTLists xs ys = (demand len xs, demand len ys)
    where
        len = max (length xs) (length ys)
        demand n (x:xs) = demand (n-1) xs
        demand n [] = replicate n TAny


-- A Subst is a mapping from variables to subtype values
type Subst = [(String, TSubtype)]

-- apply a substitution, there should be no free variables
-- in the value that do not have a mapping
applySubst :: Subst -> TSubtype -> TSubtype
applySubst dat x = mapTSubtype f x
    where
        f (TFree ns@(_:_)) = unionList $ map g ns
        f x = x
        
        g i = case lookup i dat of
                   Nothing -> TFree [i]
                   Just x -> x


validSubst :: Subst -> Bool
validSubst = all allEqual . groupSetExtract fst


{-
    Property:
    
    for each substitution $, $(LHS) `subset` RHS
    only return coherent substitutions
    - a valid substitution will map everything to something different
-}

subst :: TSubtype -> TSubtype -> [Subst]
subst lhs rhs = filter validSubst $ getSubst lhs rhs



-- roughly x `isTSubset` y (for constructors at least)
-- figure what a variable in x would have to be mapped to
getSubst :: TSubtype -> TSubtype -> [Subst]
getSubst (TBind (x:xs)) (TBind (y:ys)) =
        getSubstList $ substTopPair x y : zipWith substBotPair xs ys
    where
        substTopPair :: TPair -> TPair -> [Subst]
        substTopPair (TPair a1 b1) (TPair a2 b2) = 
            if a1 `subset` a2 then substList b1 b2 else []

        substBotPair :: TPair -> TPair -> [Subst]
        substBotPair (TPair a1 b1) (TPair a2 b2) =
            if a1 `setEq` a2 then substList b1 b2 else []
        
        substList a b = getSubstList $ zipWith getSubst a b

getSubst (TFree []) _ = [[]]
getSubst (TFree []) _ = [[]]
getSubst (TFree []) TBot = []
getSubst TBot TBot = [[]]
getSubst _ (TFree []) = [[]]
getSubst (TFree [a]) x = [[(a,x)]]
getSubst TBot (TFree [a]) = []


getSubst (TFunc []) _ = [[]]

getSubst (TFunc lhs) (TFunc rhs) = concatMap f lhs
    where
        f (TArr x xs) = concat [getSubstList $ zipWith getSubst x y ++ [getSubst xs ys] | TArr y ys <- rhs]


--getSubst (TFunc l) (TFunc r) = liftM concat $ sequence $ map f l
--    where
--        f (TArr x y) = 



--getSubst (TArr a1 b1) (TArr a2 b2) =
--    liftM concat $ sequence $ zipWithEq getSubst (b1:a1) (b2:a2)

getSubst x y = []

getSubst x y = error $ show ("getSubst",x,y)


getSubstList :: [[Subst]] -> [Subst]
getSubstList x = liftM concat $ sequence x




instance Union TSubtype where
    unionPair (TFree a) (TFree b) = TFree (a `union` b)
    unionPair (TBind a) (TBind b) = TBind $ zipWithEq unionPair a2 b2
        where (a2,b2) = compatTPairs a b
    unionPair a b = f False a b
        where
            f c TBot _ = TBot
            f c TAny x = x
            f c TVoid x = x
            f False a b = f True b a
            f True a b = error $ "Union TSubtype: " ++ show a ++ " vs " ++ show b


instance Union TPair where
    unionPair (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWithEq unionPair b12 b22)
        where (b12, b22) = compatTLists b1 b2


{-
-- Currently not required.

isTSubset :: TSubtype -> TSubtype -> Bool
isTSubset (TBind xs) (TBind ys) | length xs > length ys = False
                                | otherwise = and $ zipWith isTSubsetPair xs ys
isTSubset TBot TBot = True
isTSubset TBot _ = False
isTSubset (TFree xs) (TFree ys) = null $ xs \\ ys
isTSubset (TFree x) _ = True
isTSubset x y = error $ show ("isTSubset",x,y)


isTSubsetPair (TPair x1 y1) (TPair x2 y2) = f x1 x2 && and (zipWithEq isTSubset y1 y2)
    where
        f xs ys = null $ xs \\ ys
-}



