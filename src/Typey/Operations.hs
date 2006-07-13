
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
        demand n (x:xs) = x : demand (n-1) xs
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

-- should the substitution be checked at runtime
checkSubst = True

subst :: TSubtype -> TSubtype -> [Subst]
subst lhs rhs | checkSubst && not (null no) = error $ "subst generated invalid, " ++ show (lhs, rhs, no)
              | checkSubst && lhs `isTSubset` rhs && null res = error $ "subst missed some, " ++ show (lhs,rhs)
              | otherwise = res
    where
        (yes,no) = partition check res
        check subst = applySubst subst lhs `isTSubset` rhs
        
        -- the results
        res = filter validSubst $ getSubst lhs rhs



-- roughly x `isTSubset` y (for constructors at least)
-- figure what a variable in x would have to be mapped to
getSubst :: TSubtype -> TSubtype -> [Subst]

-- first handle bottom, only bottom is a subset of bottom
getSubst x TBot = if x == TBot then [[]] else []

getSubst TAny x = [[]]
getSubst (TFree [a]) x = [[(a,x)]]

getSubst (TBind (x:xs)) (TBind (y:ys)) =
        getSubstList $ substTopPair x y : zipWith substTopPair xs ys
    where
        substTopPair :: TPair -> TPair -> [Subst]
        substTopPair (TPair a1 b1) (TPair a2 b2) = 
            if a1 `subset` a2 then substList b1 b2 else []

        substBotPair :: TPair -> TPair -> [Subst]
        substBotPair (TPair a1 b1) (TPair a2 b2) =
            if a1 `setEq` a2 then substList b1 b2 else []
        
        substList a b = getSubstList $ zipWithEq getSubst a b

getSubst (TFunc lhs) (TFunc rhs) = getSubstList $ map f lhs
    where
        f l = concatMap (g l) rhs
        g (TArr l1 l2) (TArr r1 r2) = getSubstList $ zipWithEq getSubst (l2:l1) (r2:r1)
        

-- this is a possibility for errors to creep in
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


-- only required for checking subst
isTSubset :: TSubtype -> TSubtype -> Bool
isTSubset a b | a == b = True
isTSubset (TBind xs) (TBind ys) = isTSubsetPair subset x2 y2 && and (zipWithEq (isTSubsetPair subset) xs2 ys2)
    where (x2:xs2,y2:ys2) = compatTPairs xs ys
isTSubset (TFree xs) (TFree ys) = xs `subset` ys
isTSubset (TFunc xs) (TFunc ys) = all (\x-> any (f x) ys) xs
    where f (TArr x1 x2) (TArr y1 y2) = and $ zipWithEq isTSubset (x2:x1) (y2:y1)
isTSubset TAny (TFree xs) = True
isTSubset _ _ = False

isTSubsetPair eq (TPair x1 y1) (TPair x2 y2) = (x1 `eq` x2) && and (zipWithEq isTSubset y12 y22)
    where (y12, y22) = compatTLists y1 y2
