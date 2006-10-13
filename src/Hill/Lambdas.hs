
module Hill.Lambdas(addLambdas, moveLambdas) where

import Hill.Type


---------------------------------------------------------------------

-- add lambdas
addLambdas :: Hill -> Hill
addLambdas hill = mapOverHill f hill
    where
        f (Fun x) = mkLambda (length $ funcArgs $ getFunc hill x) (Fun x)
        f x = x

---------------------------------------------------------------------

-- move lambdas
moveLambdas :: Hill -> Hill
moveLambdas hill = mapOverHill f hill
    where
        f (Apply (Lambda n x) xs) = mkLambda (n - length xs) (Apply x xs)
        f x = x


