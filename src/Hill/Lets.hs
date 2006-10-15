
module Hill.Lets(addLetsFunc, addLets, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State


cmdsLets = [hillCmdPure "add-let" (const addLets)]


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
