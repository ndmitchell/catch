
module Data.Proposition.Simple where

import Data.Proposition.Internal
import Control.Monad



data PropSimple a = Atom a
                  | And [PropSimple a]
                  | Or  [PropSimple a]


instance Prop PropSimple where
    propTrue = And []
    propFalse = Or []
    
    propIsTrue (Atom _) = False
    propIsTrue (And xs) = all propIsTrue xs
    propIsTrue (Or  xs) = any propIsTrue xs
    
    propIsFalse (Atom _) = False
    propIsFalse (And xs) = any propIsFalse xs
    propIsFalse (Or  xs) = all propIsFalse xs
    
    propLit = Atom
    propAnd a b = And [a,b]
    propOr  a b = Or  [a,b]
    
    propNot (Atom a) = Atom $ litNot a
    propNot (And xs) = Or  $ map propNot xs
    propNot (Or  xs) = And $ map propNot xs

    propMapM f (Atom a) = f a
    propMapM f (And xs) = liftM And $ mapM (propMapM f) xs
    propMapM f (Or  xs) = liftM Or  $ mapM (propMapM f) xs
    
    propAll (Atom a) = [a]
    propAll (And xs) = concatMap propAll xs
    propAll (Or  xs) = concatMap propAll xs


