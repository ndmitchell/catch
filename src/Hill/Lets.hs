
module Hill.Lets(addLetsExpr, addLets, cmdsLets) where

import Hill.Type
import General.General


cmdsLets = [hillCmdPure "add-let" (const addLets)]


---------------------------------------------------------------------

-- add lets
-- can only do a call from a let
addLets :: Hill -> Hill
addLets hill = hill{funcs = [x{body = addLetsExpr (body x)} | x <- funcs hill]}

addLetsExpr :: Expr -> Expr
addLetsExpr x = f (freshFree x) x
    where
        f (x:xs) orig@(Apply y ys) = Let [(x, Apply (f xs y) (map (f xs) ys))] (Var x)
        f frees orig = setChildren orig $ map (f frees) $ getChildren orig

