
module Hill.ArityRaise(cmdsArityRaise) where

import Hill.Type
import Hill.Lambdas
import Data.List


cmdsArityRaise = [hillCmdPure "arity-raise" (const arityRaise)]


---------------------------------------------------------------------

arityRaise = f . addLambdas
    where
        f x = case liftLambdas y of
                   Nothing -> y
                   Just x -> f x
            where y = moveLambdas x


-- move the Lambda's, if you call someone who has a Lambda
-- then move it into your space
liftLambdas :: Hill -> Maybe Hill
liftLambdas hill | null lift = Nothing
                 | otherwise = Just hill{funcs = map g $ mapOverHill f (funcs hill)}
    where
        lift = [(funcName func, arity) | func <- funcs hill, let arity = getArity (body func), arity /= 0]
    
        
        f (Fun x) = case lookup x lift of
                        Nothing -> Fun x
                        Just n -> Lambda n (Fun x)
        f x = x
        
        g (Func name args (Lambda n body)) = Func name (args++newArgs) (mkApply body (map Var newArgs))
            where newArgs = take n $ freshFree body \\ args
        g x = x

        getArity (Lambda n x) = n
        getArity _ = 0

