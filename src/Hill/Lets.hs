
module Hill.Lets(addLetsFunc, addLetsExpr, letInline, uniqueLets, uniqueLetsExpr, addLets, topLets, topLetsExpr, cmdsLets) where

import Hill.Type
import General.General
import Control.Monad.State
import Data.List


cmdsLets = [hillCmdPure "add-let" (const addLets)
           ,hillCmdPure "top-let" (const topLets)
           ,hillCmdPure "nub-let" (const nubLets)
           ,hillCmdPure "let-inline-simp" (const letInlineSimp)
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
        liftLet (Sel _ _) = True
        liftLet _ = False



uniqueLets :: Hill -> Hill
uniqueLets hill = hill{funcs = map f (funcs hill)}
    where
        f func = func{body = uniqueLetsExpr (funcArgs func) (body func)}


uniqueLetsExpr :: [Int] -> Expr -> Expr
uniqueLetsExpr args x = evalState (mapOverM f x) (freshFree x \\ args)
    where
        f (Let binds x) = do
            free <- get
            let (used, rest) = splitAt (length binds) free
                (lhs,rhs) = unzip binds
            put rest
            return $ Let (zip used rhs) $ replaceFree (zip lhs (map Var used)) x
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



letInline :: ManipulateHill hill => hill -> hill
letInline x = mapOverHill f x
    where
        f (Let binds y) = replaceFree binds y
        f x = x


letInlineSimp :: Hill -> Hill
letInlineSimp x = mapOverHill f x
    where
        f (Let binds x) | not $ null inline = mkLet keep (replaceFree inline x)
            where (inline, keep) = partition (isSimp . snd) binds
        f x = x
        
        isSimp (Make _ _) = True
        isSimp x = isVarSel x


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


nubLets :: Hill -> Hill
nubLets hill = mapOverHill f hill
    where
        f (Let binds x) | null rens = Let binds (f x)
                        | otherwise = Let (map head grps) (f $ replaceFree rens x)
            where 
                grps = groupSetExtract snd binds
                rens = concatMap g grps
                
                g [x] = []
                g (x:xs) = zip (map fst xs) (repeat $ Var $ fst x)

        f x = x
