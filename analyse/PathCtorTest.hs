
module PathCtorTest where

import PathCtor
import SmallCheck



normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x

rePathCtor (PathCtor core path ctors) = newPathCtor core path ctors



correct_newPathCtor :: PathCtor -> Bool
correct_newPathCtor orig =
        if equalPathCtor orig new then True else error $ show (orig, new)
    where new = normalPath $ rePathCtor orig


confluent_newPathCtor :: PathCtor -> PathCtor -> Property
confluent_newPathCtor a b = equalPathCtor a b ==> rePathCtor a == rePathCtor b
