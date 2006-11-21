
module Data.Proposition.BDD(BDD) where

import Data.Proposition.Internal
import Data.BDD
import qualified Data.Map as Map


instance Prop BDD where
    propTrue  = AtomTrue
    propFalse = AtomFalse
    
    propIsTrue  AtomTrue  = True; propIsTrue  _ = False
    propIsFalse AtomFalse = True; propIsFalse _ = False

    propLit a = Choice a AtomFalse AtomTrue

    propNot AtomTrue  = AtomFalse
    propNot AtomFalse = AtomTrue
    propNot (Choice a f t) = Choice a (propNot f) (propNot t)

    propAnd = mergeWith propIsTrue  propIsFalse 
    propOr  = mergeWith propIsFalse propIsTrue

    propMapM = mapBDDM

    propSimplify = bddSimplify (?=>) . bddApplyAnd liftAnd
        where
            liftAnd a b = case a ?/\ b of
                               Value x -> Just x
                               _ -> Nothing


---------------------------------------------------------------------
-- MERGING, FOR OR/AND

mergeWith :: Ord a => (BDD a -> Bool) -> (BDD a -> Bool) -> BDD a -> BDD a -> BDD a
mergeWith ignore promote c1 c2
    | ignore c1 = c2
    | ignore c2 = c1
    | promote c1 = c1
    | promote c2 = c2

mergeWith ignore promote c1@(Choice a1 f1 t1) c2@(Choice a2 f2 t2) =
    case compare a1 a2 of
        EQ -> choice a1 (cont f1 f2) (cont t1 t2)
        LT -> choice a1 (cont f1 c2) (cont t1 c2)
        GT -> choice a2 (cont c1 f2) (cont c1 t2)
    where
        cont = mergeWith ignore promote


---------------------------------------------------------------------
-- MAPPING

mapBDDM :: (Show a, Monad m, Ord a) => (a -> m (BDD a)) -> BDD a -> m (BDD a)
mapBDDM app x = do
        (d, res) <- g app x Map.empty
        return $ rebalance res
    where

        g app (Choice a f0 t0) cache = do
            (cache,a2) <- case Map.lookup a cache of
                Just a2 -> return (cache,a2)
                Nothing -> do
                    a2 <- app a
                    return (Map.insert a a2 cache,a2)

            case a2 of
                AtomTrue -> g app t0 cache
                AtomFalse -> g app f0 cache
                Choice a2 f1 t1 -> do
                    (cache,f0) <- g app f0 cache
                    (cache,t0) <- g app t0 cache
                    return (cache, Choice a2 (h f1 f0 t0) (h t1 f0 t0))


        g app x cache = return (cache,x)

        -- replace all occurances of AtomTrue/AtomFalse with the given predicate
        h rep f t = case rep of
            AtomTrue -> t
            AtomFalse -> f
            Choice a f1 t1 -> Choice a (h f1 f t) (h t1 f t)
