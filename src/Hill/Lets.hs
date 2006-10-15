
module Hill.Lets(addLetsFunc, addLets, topLets, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State


cmdsLets = [hillCmdPure "add-let" (const addLets)
           ,hillCmdPure "top-let" (const topLets)]


---------------------------------------------------------------------

-- add lets
-- can only do a call from a let
addLets :: Hill -> Hill
addLets hill = hill{funcs = map addLetsFunc (funcs hill)}

addLetsFunc :: Func -> Func
addLetsFunc x = x{body = evalState (mapOverM f (body x)) (freshFreeFunc x)}
    where
        f (Apply y ys) = do
            (x:xs) <- get
            put xs
            return $ Let [(x, Apply y ys)] (Var x)
        f x = return x

---------------------------------------------------------------------

-- move all let expressions to the very top of a function
topLets :: Hill -> Hill
topLets hill = mapOverHill f hill
    where
        f orig@(Let _ _) = orig
        f x = mkLet (concat lhs) $ setChildren x rhs
            where
                (lhs,rhs) = unzip $ map g $ getChildren x
                
                g (Let binds x) = (binds,x)
                g x = ([], x)
