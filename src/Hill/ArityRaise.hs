
module Hill.ArityRaise(cmdsArityRaise) where

import Hill.Type
import Hill.Lambdas
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

arityCallers :: Hill -> Hill
arityCallers hill = hill{funcs = evalState (driver (funcs hill)) (calcUnique hill, Map.empty)}
    where
        driver :: [Func] -> State ArityCallersState [Func]
        driver [] = return []
        driver funcs =
            do
                funcs2 <- mapM (\x -> do body2 <- addRequests (body x) ; return x{body=body2}) funcs
                (n,mp) <- get
                let newfuncs = [genFunc oldname arity newname | ((oldname,arity),(newname,False)) <- Map.toList mp]
                put (n, Map.map (\x -> (fst x, True)) mp)
                newfuncs2 <- driver newfuncs
                return $ funcs2 ++ newfuncs2
        
        
        -- convert an expression, adding the items required
        addRequests :: Expr -> State ArityCallersState Expr
        addRequests x = mapOverM f x
            where
                f (Apply (Fun x) xs) | length xs > nargs = do
                        (n,mp) <- get
                        case Map.lookup (x, nxs) mp of
                            Just q -> return $ Apply (Fun (fst q)) xs
                            Nothing -> do
                                let newname = genUnique x n
                                put (n+1, Map.insert (x,nxs) (newname,False) mp)
                                return $ Apply (Fun newname) xs
                    where
                        nxs = length xs
                        nargs = length $ funcArgs $ getFunc hill x
                
                f x = return x

        
        -- add the functions that are required
        genFunc :: FuncName -> Int -> FuncName -> Func
        genFunc name arity newname =
                Func newname (args++free) $ simplify hill $ Apply body (map Var free)
            where
                free = take (arity - length args) $ freshFreeFunc func
                func@(Func _ args body) = getFunc hill name