
module PathCtorTest where

import PathCtor
import PathCtorEq
import SmallCheck
import Data.Proposition



normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x

rePathCtor (PathCtor core path ctors) = newPathCtor core path ctors



correct_newPathCtor :: PathCtor -> Bool
correct_newPathCtor orig =
        if equalPathCtor orig new then True else error $ show (orig, new)
    where new = normalPath $ rePathCtor orig


confluent_newPathCtor :: PathCtor -> PathCtor -> Property
confluent_newPathCtor a b = equalPathCtor a b ==> rePathCtor a == rePathCtor b



-- put the items in a box, so normal simplification rules do not fire
data PropBox a = PropBox a
instance (Show a, Ord a) => PropLit (PropBox a)
instance Show a => Show (PropBox a) where show (PropBox a) = show a
instance Eq a => Eq (PropBox a) where (==) (PropBox a) (PropBox b) = (==) a b
instance Ord a => Ord (PropBox a) where compare (PropBox a) (PropBox b) = compare a b

