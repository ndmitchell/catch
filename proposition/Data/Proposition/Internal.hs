
module Data.Proposition.Internal where

import Control.Monad.Identity
import Control.Monad.State


class Prop p where
    propTrue  :: p a
    propFalse :: p a
    propIsTrue  :: PropLit a => p a -> Bool
    propIsFalse :: PropLit a => p a -> Bool
    
    propLit :: PropLit a => a -> p a
    propAnd :: PropLit a => p a -> p a -> p a
    propOr  :: PropLit a => p a -> p a -> p a
    propNot :: PropNot a => p a -> p a

    propMapM :: (Monad m, PropLit a) => (a -> m (p a)) -> p a -> m (p a)
    
    propRebuild :: (PropLit a, Prop q) => p a -> q a

    -- non essential methods
    propAnds :: PropLit a => [p a] -> p a
    propAnds = foldr propAnd propTrue
    
    propOrs :: PropLit a => [p a] -> p a
    propOrs = foldr propOr propFalse


    propMap  :: PropLit a => (a -> p a) -> p a -> p a
    propMap f = runIdentity . propMapM (return . f)

    propAll  :: PropLit a => p a -> [a]
    propAll x = execState (propMapM f x) []
        where f x = modify (x:) >> return (propLit x)
    
    propBool :: Bool -> p a
    propBool b = if b then propTrue else propFalse


data Reduce a = Value a
              | Literal Bool
              | None


class (Show a, Ord a) => PropLit a where
    (?/\) :: a -> a -> Reduce a
    (?\/) :: a -> a -> Reduce a
    (?=>) :: [(a,Bool)] -> a -> Maybe Bool
    simp :: a -> Maybe Bool
    
    a ?/\ b = None
    a ?\/ b = None
    a ?=> b = lookup b a
    simp a = Nothing


class PropLit a => PropNot a where
    litNot :: a -> a


data PropNeg a = PropNeg a
               | PropId  a
               deriving (Ord, Eq, Show)


instance PropLit a => PropNot (PropNeg a) where
    litNot (PropNeg x) = PropId  x
    litNot (PropId  x) = PropNeg x

instance PropLit a => PropLit (PropNeg a) where
    -- the properties can be implemented for this slightly different
    -- but work has not been done yet
    -- if you need it, shout!
