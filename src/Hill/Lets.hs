
module Hill.Lets(addLetsExpr, addLets, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State


cmdsLets = [hillCmdPure "add-let" (const addLets)]


---------------------------------------------------------------------

-- add lets
-- can only do a call from a let
addLets :: Hill -> Hill
addLets hill = hill{funcs = [x{body = addLetsExpr (body x)} | x <- funcs hill]}

addLetsExpr :: Expr -> Expr
addLetsExpr x = evalState (mapOverM f x) (freshFree x)
    where
        f (Apply y ys) = do
            (x:xs) <- get
            put xs
            return $ Let [(x, Apply y ys)] (Var x)
        f x = return x
