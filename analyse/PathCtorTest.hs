
module PathCtorTest where

import PathCtor
import SmallCheck



normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x



correct_newPathCtor :: PathCtor -> Bool
correct_newPathCtor orig@(PathCtor core path ctors) =
        if equalPathCtor orig new then True else error $ show (orig, new)
    where new = normalPath $ newPathCtor core path ctors
