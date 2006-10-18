
module Hill.Lets(addLetsFunc, addLetsExpr, letInline, addLets, topLets, topLetsExpr, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State
import Data.List


cmdsLets = [hillCmdPure "add-let" (const addLets)
           ,hillCmdPure "top-let" (const topLets)
           ,hillCmdPure "let-inline1" (const letInlineOnce)
           ]


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
        f orig | liftLet orig = do
            (x:xs) <- get
            put xs
            return $ Let [(x, orig)] (Var x)

        f x = return x
        
        
        liftLet (Apply _ _) = True
        liftLet (Call _ _) = True
        liftLet (Prim _ _) = True
        liftLet _ = False

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



letInline :: ManipulateHill hill => hill -> hill
letInline x = mapOverHill f x
    where
        f (Let binds y) = replaceFree binds y
        f x = x



letInlineOnce :: ManipulateHill hill => hill -> hill
letInlineOnce x = mapOverHill f x
    where
        f (Let binds x) = mkLet leave $ replaceFree inline x
            where
                (inline,leave) = partition (\x -> fst x `elem` once) binds
            
                once = sused \\ snub (used \\ sused)
                sused = snub used
                used = [i | Var i <- allOverHill x]
        f x = x
