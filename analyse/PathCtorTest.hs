
module PathCtorTest where

import PathCtor hiding (equalPathCtor)
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


eqPathCtor :: PathCtor -> PathCtor -> Bool
eqPathCtor a b = eq (propLit $ PropBox a) (propLit $ PropBox b)

eq :: Formula (PropBox PathCtor) -> Formula (PropBox PathCtor) -> Bool
eq p1 p2 = all test [("A",["as","a"]), ("B",["bs"]), ("C",["c"]), ("D",[])]
    where
        test (ctr,fields) = case (b1,b2) of
                                 (Just a1, Just a2) | a1 /= a2 -> error $ show (p1, p2) -- False
                                                    | otherwise -> if a1 then rest else True
                                 _ -> rest
            where
                rest = all follow fields
                (b1, b2) = (evaluate ctr p1, evaluate ctr p2)
            
        evaluate ctr p = if propIsTrue res then Just True
                         else if propIsFalse res then Just False
                         else Nothing
            where
                res = propMap f p
            
                f (PropBox (PathCtor core path ctors)) | ewpPath path && ctr `notElem` ctors = propFalse
                f x = propLit x
        
        follow field = (d1 == p1 && d2 == p2) || eq d1 d2
            where (d1, d2) = (diff field p1, diff field p2)
        
        diff field = propMap f
            where
                f (PropBox (PathCtor core path ctors)) =
                    case differentiate path field of
                        Nothing -> propTrue
                        Just x -> propLit $ PropBox $ PathCtor core x ctors
