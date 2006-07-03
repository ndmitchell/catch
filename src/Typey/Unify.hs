
module Typey.Unify(unify, applyUnify) where

import Typey.Type
import Typey.Subtype
import General.General
import Data.List
import Data.Maybe
import Control.Monad


type Subst = [(String, TSubtype)]


applyUnify :: Subst -> TSubtype -> TSubtype
applyUnify dat (TBind xs) = TBind $ map f xs
    where
        f (TPair a1 b1) = TPair a1 (map (applyUnify dat) b1)

applyUnify dat (TFree []) = TFree []
applyUnify dat o@(TFree [n]) = case lookup n dat of
                                          Nothing -> o
                                          Just x -> x
applyUnify dat (TFree ns) = unionList $ map (applyUnify dat . TFree . box) ns
applyUnify dat (TFunc x) = tFunc $ map f x
    where f (TArr a b) = TArr (map (applyUnify dat) a) (applyUnify dat b)
applyUnify dat TBot = TBot
applyUnify dat x = error $ show ("applyUnify",dat,x)



-- roughly x `isTSubset` y (for constructors at least)
-- figure what a variable in x would have to be mapped to
unify :: TSubtype -> TSubtype -> [Subst]
unify (TBind (x:xs)) (TBind (y:ys)) = unifyList $ f x y : zipWith g xs ys
    where
        f (TPair a1 b1) (TPair a2 b2) =
            if null $ a1 \\ a2
            then unifyList $ zipWith unify b1 b2
            else []

        -- demand equality for child types, because of the powerset rule
        -- kinda hacky, needs formalising
        g p1@(TPair a1 b1) p2@(TPair a2 b2) =
            if a1 `setEq` a2 then f p1 p2 else []
            
unify (TFree []) _ = [[]]
unify (TFree []) TBot = []
unify TBot (TFree []) = []
unify TBot TBot = [[]]
unify _ (TFree []) = [[]]
unify (TFree [a]) x = [[(a,x)]]
unify TBot (TFree [a]) = []


unify (TFunc []) _ = [[]]

unify (TFunc lhs) (TFunc rhs) = concatMap f lhs
    where
        f (TArr x xs) = concat [unifyList $ zipWith unify x y ++ [unify xs ys] | TArr y ys <- rhs]


--unify (TFunc l) (TFunc r) = liftM concat $ sequence $ map f l
--    where
--        f (TArr x y) = 



--unify (TArr a1 b1) (TArr a2 b2) =
--    liftM concat $ sequence $ zipWithEq unify (b1:a1) (b2:a2)


unify x y = error $ show ("unify",x,y)


unifyList :: [[Subst]] -> [Subst]
unifyList x = liftM concat $ sequence x
