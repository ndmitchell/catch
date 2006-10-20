
module Hill.ArityRaise(cmdsArityRaise) where

import Hill.Type
import Hill.Lambdas
import Hill.Producer
import Hill.Simple
import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import General.General


cmdsArityRaise = [hillCmdPure "arity-raise" (const arityRaise)
                 ,hillCmdPure "arity-callers" (const arityCallers)]


---------------------------------------------------------------------

arityRaise = remLambdas . f . addLambdas
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



-- if you can Fun with 3 arguments, ensure that Fun
-- takes a minimum of 3 arguments

type ArityCallersState = (Int, Map.Map (FuncName, Int) (FuncName, Bool))

type Spec = (FuncName,Int)

arityCallers :: Hill -> Hill
arityCallers hill = hill{funcs = funcs2}
    where
        funcs2 = fst $ producer hill (funcs hill) processor generator
        
        processor :: Monad m => (Spec -> m FuncName) -> Func -> m Func
        processor ask x = do bod <- mapOverM f $ body x ; return x{body = bod}
            where
                f (Apply (Fun x) xs) | length xs > nargs = do
                    x2 <- ask (x,nxs)
                    return $ Apply (Fun x2) xs
                    where
                        nxs = length xs
                        nargs = length $ funcArgs $ getFunc hill x
                
                f x = return x

        generator :: Spec -> Int -> Func
        generator (name,arity) idn = 
                Func newname (args++free) $ simplify hill $ Apply body (map Var free)
            where
                newname = genUnique name idn
                free = take (arity - length args) $ freshFreeFunc func
                func@(Func _ args body) = getFunc hill name
