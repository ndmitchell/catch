
module Typey.Operations(subst, applySubst) where

import Typey.Type
import Typey.Subtype
import General.General
import Data.List
import Data.Maybe
import Control.Monad


type Subst = [(String, TSubtype)]


applySubst :: Subst -> TSubtype -> TSubtype
applySubst dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applySubst dat) b1)

applySubst dat (TFree []) = TFree []
applySubst dat o@(TFree [n]) = case lookup n dat of
                                          Nothing -> o
                                          Just x -> x
applySubst dat (TFree ns) = unionList $ map (applySubst dat . TFree . box) ns
applySubst dat (TFunc x) = tFunc $ map f x
    where f (TArr a b) = TArr (map (applySubst dat) a) (applySubst dat b)
applySubst dat TBot = TBot
applySubst dat x = error $ show ("applySubst",dat,x)



-- roughly x `isTSubset` y (for constructors at least)
-- figure what a variable in x would have to be mapped to
subst :: TSubtype -> TSubtype -> [Subst]
subst (TBind (x:xs)) (TBind (y:ys)) = substList $ f x y : zipWith g xs ys
    where
        f (TPair a1 b1) (TPair a2 b2) =
            if null $ a1 \\ a2
            then substList $ zipWith subst b1 b2
            else []

        -- demand equality for child types, because of the powerset rule
        -- kinda hacky, needs formalising
        g p1@(TPair a1 b1) p2@(TPair a2 b2) =
            if a1 `setEq` a2 then f p1 p2 else []
            
subst (TFree []) _ = [[]]
subst (TFree []) TBot = []
subst TBot (TFree []) = []
subst TBot TBot = [[]]
subst _ (TFree []) = [[]]
subst (TFree [a]) x = [[(a,x)]]
subst TBot (TFree [a]) = []


subst (TFunc []) _ = [[]]

subst (TFunc lhs) (TFunc rhs) = concatMap f lhs
    where
        f (TArr x xs) = concat [substList $ zipWith subst x y ++ [subst xs ys] | TArr y ys <- rhs]


--subst (TFunc l) (TFunc r) = liftM concat $ sequence $ map f l
--    where
--        f (TArr x y) = 



--subst (TArr a1 b1) (TArr a2 b2) =
--    liftM concat $ sequence $ zipWithEq subst (b1:a1) (b2:a2)

subst x y = []

subst x y = error $ show ("subst",x,y)


substList :: [[Subst]] -> [Subst]
substList x = liftM concat $ sequence x




instance Union TSubtype where
    unionPair (TFree a) (TFree b) = TFree (a `union` b)
    unionPair (TBind a) (TBind b) = TBind (zipWithRest unionPair a b)
    --unionPair (TArr a1 b1) (TArr a2 b2) = tArr (zipWithEq unionPair a1 a2) (b1 `unionPair` b2)
    unionPair TBot _ = TBot
    unionPair _ TBot = TBot
    unionPair (TFree []) x = x
    unionPair x (TFree []) = x
    unionPair _ _ = TFree []
    unionPair a b = error $ show ("Union TSubtype",a,b)

instance Union TPair where
    unionPair (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWithEq unionPair b1 b2)


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




