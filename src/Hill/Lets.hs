
module Hill.Lets(addLetsFunc, addLetsExpr, addLets, topLets, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State
import Data.List


cmdsLets = [hillCmdPure "add-let" (const addLets)
           ,hillCmdPure "top-let" (const topLets)]


---------------------------------------------------------------------

-- add lets
-- can only do a call from a let
addLets :: Hill -> Hill
addLets hill = hill{funcs = map addLetsFunc (funcs hill)}

addLetsFunc :: Func -> Func
addLetsFunc x = x{body = addLetsExpr (funcArgs x) (body x)}

addLetsExpr :: [Int] -> Expr -> Expr
addLetsExpr args x = evalState (mapOverM f x) (freshFree x \\ args)
    where
        f (Apply y ys) = do
            (x:xs) <- get
            put xs
            return $ Let [(x, Apply y ys)] (Var x)
        
        f (Call y ys) = do
            (x:xs) <- get
            put xs
            return $ Let [(x, Call y ys)] (Var x)
            
        f x = return x

---------------------------------------------------------------------

-- move all let expressions to the very top of a function
topLets :: ManipulateHill hill => hill -> hill
topLets hill = mapOverHill f hill
    where
        f (Let binds x) = mkLet (concat bins) (mkLet (zip lhs rhs) x)
            where
                (lhs, bins, rhs) = unzip3 $ map g binds
                
                g (a, Let x y) = (a, x, y)
                g (a, y) = (a, [], y)
        
        
        f x = mkLet (concat lhs) $ setChildren x rhs
            where
                (lhs,rhs) = unzip $ map g $ getChildren x
                
                g (Let binds x) = (binds,x)
                g x = ([], x)
