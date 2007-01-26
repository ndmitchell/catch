
module Data.Proposition.Internal where

import Control.Monad.Identity
import Control.Monad.State


data PropFold lit res = PropFold {foldOr  :: [res] -> res, foldAnd :: [res] -> res,
                                  foldNot :: res -> res  , foldLit :: lit -> res  }


class Prop p where
    propTrue  :: p a
    propFalse :: p a
    propIsTrue  :: PropLit a => p a -> Bool
    propIsFalse :: PropLit a => p a -> Bool
    
    propLit :: PropLit a => a -> p a
    propAnd :: PropLit a => p a -> p a -> p a
    propOr  :: PropLit a => p a -> p a -> p a
    propNot :: PropLit a => p a -> p a

    propMapM :: (Monad m, PropLit a) => (a -> m (p a)) -> p a -> m (p a)
    
    propFold :: PropLit a => PropFold a res -> p a -> res
    
    -- non essential methods
    propAnds :: PropLit a => [p a] -> p a
    propAnds = foldr propAnd propTrue
    
    propOrs :: PropLit a => [p a] -> p a
    propOrs = foldr propOr propFalse

    propRebuild :: (PropLit a, Prop q) => p a -> q a
    propRebuild = propFold (PropFold propOrs propAnds propNot propLit)

    propMap  :: PropLit a => (a -> p a) -> p a -> p a
    propMap f = runIdentity . propMapM (return . f)

    propAll  :: PropLit a => p a -> [a]
    propAll x = execState (propMapM f x) []
        where f x = modify (x:) >> return (propLit x)
        
    propSimplify :: PropLit a => p a -> p a
    propSimplify = id
    
    propBool :: Bool -> p a
    propBool b = if b then propTrue else propFalse


data Reduce a = Value a
              | Literal Bool
              | None
              deriving (Show,Eq)

-- based on the instance for Maybe
instance Monad Reduce where
    Value x   >>= k = k x
    None      >>= k = None
    Literal x >>= k = Literal x
    return        = Value
    fail s        = None


class (Show a, Ord a) => PropLit a where
    (?/\) :: a -> a -> Reduce a
    (?\/) :: a -> a -> Reduce a
    (?=>) :: [(a,Bool)] -> a -> Maybe Bool
    simp :: a -> Maybe Bool
    litNot :: a -> Maybe a
    
    a ?/\ b = None
    a ?\/ b = None
    a ?=> b = lookup b a
    simp a = Nothing
    litNot a = Nothing




-- | Try and simplify an Or, using implies
reduceOrWithImp :: PropLit a => a -> a -> Maybe (Reduce a)
{-
a b | a => b | a v b | a v b == b
F F     T        F        T
F T     T        T        T
T F     F        T        ?
T T     T        T        T 
-}
reduceOrWithImp a b
    | [(a,True)] ?=> b == Just True = Just $ Value b
    | [(b,True)] ?=> a == Just True = Just $ Value a
    | otherwise = Nothing



-- | Try and simplify an And, using implies
reduceAndWithImp :: PropLit a => a -> a -> Maybe (Reduce a)
{-
a b | a => b | a ^ b | a ^ b == a
F F     T        F        T
F T     T        F        T
T F     F        F        ?
T T     T        T        T

a b | a => ¬b | a ^ b | a ^ b == F
F F      T        F        T
F T      T        F        T
T F      T        F        T
T T      F        T        ?
-}
reduceAndWithImp a b
    | a_implies_b == Just True  = Just $ Value a
    | a_implies_b == Just False = Just $ Literal False
    | b_implies_a == Just True  = Just $ Value b
    | b_implies_a == Just False = Just $ Literal False
    | otherwise = Nothing
    where
        a_implies_b = [(a,True)] ?=> b
        b_implies_a = [(b,True)] ?=> a
