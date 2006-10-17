
module Hill.Lets(addLetsFunc, addLetsExpr, addLets, topLets, topLetsExpr, cmdsLets) where

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
topLets :: Hill -> Hill
topLets hill = hill{funcs = [func{body = topLetsExpr (body func)} | func <- funcs hill]}

topLetsExpr :: Expr -> Expr
topLetsExpr x = pickBinds (map fst binds) binds $ noLet x
    where
        binds = [(l, noLet r) | Let b _ <- allOverHill x, (l,r) <- b]
        
        noLet = mapOverHill f
            where
                f (Let _ x) = x
                f x = x

        pickBinds bvars [] x = x
        pickBinds bvars binds x = if null top then error "Recursive let screwed up, Hill.Lets.topLetsExpr"
                                  else mkLet top (pickBinds (bvars \\ map fst top) lower x)
            where
                (top,lower) = partition (isTop . snd) binds
                isTop x = [v | Var v <- allOverHill x] `disjoint` bvars