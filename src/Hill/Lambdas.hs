
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
        f lhs@(Apply (Lambda n x) xs) = mkApply (mkLambda (n - length a) (Apply x a)) b
            where (a,b) = splitAt n xs
        
        f (Case on alts) = mkLambda arity (Case on [alt{altExpr = dropLambda (altExpr alt)} | alt <- alts])
            where arity = maximum (0:[n | Lambda n _ <- map altExpr alts])

        f x = x


