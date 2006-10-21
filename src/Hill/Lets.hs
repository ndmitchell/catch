
module Hill.Lets(
    cmdsLets,
    addLetsFunc, addLetsExpr,
    letInline, letInlineSimp, letInlineOnce,
    uniqueLets, nubLets, uniqueLetsExpr,
    addLets,
    topLets, topLetsExpr,
    letNormalForm, letNormalFormFunc, letNormalFormExpr,
    letLinearForm
    ) where

import Hill.Type
import General.General
import Control.Monad.State
import Control.Exception
import Data.List


cmdsLets = [hillCmdPure "add-let" (const addLets)
           ,hillCmdPure "top-let" (const topLets)
           ,hillCmdPure "nub-let" (const nubLets)
           ,hillCmdPure "let-inline-simp" (const letInlineSimp)
           ,hillCmdPure "let-inline1" (const letInlineOnce)
           ,hillCmdPure "let-nf" (const letNormalForm)
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


letInlineSimp :: ManipulateHill hill => hill -> hill
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


nubLets :: ManipulateHill hill => hill -> hill
nubLets hill = mapOverHill f hill
    where
        f (Let binds x) | null rens = Let binds (f x)
                        | otherwise = Let (map head grps) (f $ replaceFree rens x)
            where 
                grps = groupSetBy eq binds
                rens = concatMap g grps
                
                eq (_,a) (_,b) = isVarSel a && a == b
                
                g [x] = []
                g (x:xs) = zip (map fst xs) (repeat $ Var $ fst x)

        f x = x


---------------------------------------------------------------------


letNormalForm :: Hill -> Hill
letNormalForm hill = hill{funcs = map (letNormalFormFunc hill []) (funcs hill)}


letNormalFormFunc :: Hill -> [Int] -> Func -> Func
letNormalFormFunc hill avoid (Func name args body) = Func name newargs bod
    where
        bod = letNormalFormExpr hill (avoid2++newargs) $ replaceFree (zip args (map Var newargs)) body
        
        newargs = take (length args) $ [1..] \\ avoid2
        avoid2 = snub $ avoid ++ usedFree body ++ args


letNormalFormExpr :: Hill -> [Int] -> Expr -> Expr
letNormalFormExpr hill avoid x = x4
    where
        x4 = letSpecialise $ letOrder $ commonSub $ alwaysTop x3
        (x3,free3) = alwaysLet free2 x2
        (x2,free2) = uniqueLet free $ fullLet x
    
        free = [1..] \\ avoid
        
        
        -- replace Let (b:bs) in x ==> Let b (Let bs in x)
        fullLet x = mapOverHill f x
            where
                f (Let (b:bs) x) = Let [b] $ f $ mkLet bs x
                f x = x
        
        
        uniqueLet free x = runState (mapOverM f x) free
            where
                f (Let binds x) = do
                    free <- get
                    let (used, rest) = splitAt (length binds) free
                        (lhs,rhs) = unzip binds
                    put rest
                    return $ Let (zip used rhs) $ replaceFree (zip lhs (map Var used)) x
                f x = return x


        alwaysLet free x = runState (mapOverM f x) free
            where
                f (Var x) = return $ Var x
            
                f orig = do
                    (x:xs) <- get
                    put xs
                    return $ Let [(x, orig)] (Var x)


        alwaysTop x = Let binds bod
            where
                binds = [(lhs,elimLet rhs) | Let bs _ <- allOverHill x, (lhs,rhs) <- bs]
                bod = dropLet x


        commonSub (Let binds x)
                | not $ null rep  = commonSub $ replaceFree rept $ mkLet keep x
            where
                (simp,complex) = partition (isVar . snd) binds
                keep = map head groups
                rep = simp ++ [(fst r, Var (fst g)) | (g:rp) <- groups, r <- rp]

                groups = groupSetBy f complex
                f (_,r1) (_,r2) = r1 == r2 && (isMake r1 || isVarSel r1)

                rept = [(a,g b) | (a,b) <- rep]
                g (Var i) = case lookup i rep of
                                Just x -> g x
                                Nothing -> Var i
                g x = x

        commonSub x = x
        
        
        letOrder (Let binds inside) = assert (not $ null top) $ Let (top++lower) inside2
            where
                (lower,inside2) = case letOrder $ mkLet other inside of
                                       Let a b -> (a,b)
                                       a -> ([],a)
                
                (top,other) = partition g binds
                g y = not $ any (`elem` lhss) [i | Var i <- allOverHill $ snd y]
                lhss = map fst binds

        letOrder x = x


        -- do not case or sel on something you know
        letSpecialise (Let binds inside) | not $ null reps =
                letSpecialise $ replaceFree reps (Let newbinds inside)
            where
                newbinds = filter (\x -> not $ fst x `elem` map fst reps) binds
                reps = concatMap f binds
                
                f (lhs,Sel (Var x) path) =
                    case lookup x binds of
                        Just (Make y ys) | y == ctorName carg -> [(lhs,ys !! cargPos carg)]
                            where carg = getCArg hill path
                        _ -> []
                
                f (lhs,Case (Var x) alts) =
                    case lookup x binds of
                        Just (Const a) | length poss == 1 -> [(lhs, altExpr $ head poss)]
                            where poss = filterAltsConst a alts
                        Just (Make x _) | length poss == 1 -> [(lhs, altExpr $ head poss)]
                            where poss = filterAltsCtr x alts
                        _ -> []

                f (lhs,Var x) = [(lhs,Var x)]
                f _ = []

                
        letSpecialise x = x
        


letLinearForm :: Hill -> Hill
letLinearForm hill = mapOverHill f hill
    where
        f (Let (x:xs) y) = Let [x] $ f $ mkLet xs y
        f x = x
