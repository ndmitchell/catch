
module PathCtorTest where

import PathCtor
import PathCtorEq
import SmallCheck
import Data.Proposition
import Debug.Trace


normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x

reducePath (Value x) = x
reducePath (Literal x) = if x then truePathCtor else falsePathCtor


rePathCtor (PathCtor core path ctors) = newPathCtor core path ctors



correct_newPathCtor :: PathCtor -> Bool
correct_newPathCtor orig =
        if equalPathCtor orig new then True else error $ show (orig, new)
    where new = normalPath $ rePathCtor orig


confluent_newPathCtor :: PathCtor -> PathCtor -> Property
confluent_newPathCtor a b = equalPathCtor a b ==> rePathCtor a == rePathCtor b



correct_or :: PathCtor -> PathCtor -> Property
correct_or a b = isRight a2 && isRight b2 && res /= None ==>
                 if equalPathCtorProp propBox lhs rhs then True else error $ show (a3,b3,res)
    where
        (a2,b2) = (rePathCtor a, rePathCtor b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (box a3) (box b3)
        rhs = box (reducePath res) :: Formula (PropBox PathCtor)
        
        res = a3 ?\/ b3


confluent_or :: PathCtor -> PathCtor -> PathCtor -> Property
confluent_or a b c = isRight a2 && isRight b2 && equalPathCtorProp propBox lhs (box c) ==>
                     if ans then True else trace (show (a2,b2,rePathCtor c)) True
    where
        ans = res /= None && normalPath (rePathCtor c) == reducePath res
        (a2,b2) = (rePathCtor a, rePathCtor b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (box a3) (box b3) :: Formula (PropBox PathCtor)
        
        res = a3 ?\/ b3


-- put the items in a box, so normal simplification rules do not fire
box x = propLit (PropBox x)
data PropBox a = PropBox {propBox :: a}
instance (Show a, Ord a) => PropLit (PropBox a)
instance Show a => Show (PropBox a) where show (PropBox a) = show a
instance Eq a => Eq (PropBox a) where (==) (PropBox a) (PropBox b) = (==) a b
instance Ord a => Ord (PropBox a) where compare (PropBox a) (PropBox b) = compare a b



isRight (Right{}) = True; isRight _ = False
