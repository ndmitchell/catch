
module Hill.Lambdas(addLambdas, remLambdas, moveLambdas) where

import Hill.Type


---------------------------------------------------------------------

-- add lambdas
addLambdas :: Hill -> Hill
addLambdas hill = mapOverHill f hill
    where
        f (Fun x) = mkLambda (length $ funcArgs $ getFunc hill x) (Fun x)
        f (Lambda _ x) = x
        f x = x


remLambdas :: Hill -> Hill
remLambdas hill = mapOverHill f hill
    where
        f (Lambda _ x) = x
        f x = x

---------------------------------------------------------------------

-- move lambdas
moveLambdas :: ManipulateHill hill => hill -> hill
moveLambdas hill = mapOverHill f hill
    where
        f (Apply (Lambda n x) xs) = mkApply (mkLambda (n - length a) (Apply x a)) b
            where (a,b) = splitAt n xs
        
        f (Lambda n (Lambda m x)) = mkLambda (n+m) x
        
        f (Case on alts) = mkLambda arity (Case on [alt{altExpr = dropLambda (altExpr alt)} | alt <- alts])
            where arity = maximum (0:[n | Lambda n _ <- map altExpr alts])

        f x = x


