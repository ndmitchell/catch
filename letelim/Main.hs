
module Main where

import System.FilePath
import Yhc.Core
import System.Environment
import System.Directory
import Data.List
import Control.Monad.State


main = getArgs >>= mapM_ f
    where
        f x = do
            x2 <- findFile x
            let x3 = dropExtensions x2 <.> "letelim.yca"
            core <- loadCore x2
            let core2 = letElim core
            saveCore x3 core2
            writeFile (x3 <.> "html") (coreHtml core2)
            print core2


findFile :: String -> IO FilePath
findFile file = do
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        (x:_) -> return x
        _ -> error $ "File not found, " ++ file
    where files = file : ["../examples" </> s </> "ycr" </> file <.> "first.yca" | s <- ["Example","Nofib"]]



letElim :: Core -> Core
letElim = letKill . letMove . letAdd . caseFix . uniqueBoundVarsCore . coreReachable ["main"] . coreInline InlineForward

{-
NOTES:
Algorithm:
1 Simple cases's on variables
2 Case completion and variable inside case elimination
3 Full let addition, for all let's
4 Let movement
5 Let elimination
-}


-- case a of C -> ... a ... => case a of C -> ... C ...
-- case a of [] -> ... ; _ -> ... => case ...... (x:xs) ->
caseFix :: Core -> Core
caseFix core = coreSimplify $ applyBodyCore (mapOverCore f) core
    where
        f (CoreCase (CoreVar on) alts) = CoreCase (CoreVar on) [(a, replaceFreeVars [(on, a)] b) | (a,b) <- complete alts]
        f o@(CoreCase on alts) = CoreLet [(newvar,on)] (CoreCase (CoreVar newvar) alts)
            where newvar = head $ variableSupply 'v' \\ collectAllVars o
        f x = x
        
        -- if there is a default (last one)
        -- either remove it (complete already)
        -- or instantiate (one remaining)
        complete xs =
                if not (isCoreVar lhs) || unused > 1 then xs
                else if unused == 0 then start
                else start ++ [h $ head $ filter ((`notElem` found) . coreCtorName) total]
            where
                typ = g $ fst $ head start
                found = map (g . fst) start
                total = concat [coreDataCtors dat | dat <- coreDatas core, typ `elem` map coreCtorName (coreDataCtors dat)]
                unused = length total - length found
                (start,(lhs,rhs)) = (init xs, last xs)
                
                g (CoreApp (CoreCon x) _) = x
                g (CoreCon x) = x
                
                h (CoreCtor name fields) = (lhs2, replaceFreeVars [(fromCoreVar lhs,lhs2)] rhs)
                    where
                        lhs2 = CoreApp (CoreCon name) (map CoreVar fresh)
                        fresh = take (length fields) $ variableSupply 'v' \\ (collectAllVars lhs ++ collectAllVars rhs)


-- guarantee unique free variables
--
-- case complex of ... => let i = complex in case i of ...
-- f complex complex => let i = complex in f i i  [NOT DONE YET]
letAdd :: Core -> Core
letAdd = applyBodyCore f
    where
        f x = evalState (mapUnderCoreM g x2) vars
            where
                vars = variableSupply 'v' \\ collectAllVars x2
                x2 = uniqueBoundVarsWithout (collectAllVars x) x

        g (CoreCase (CoreVar x) xs) = return $ CoreCase (CoreVar x) xs
        g (CoreCase x xs) = do
            (y:ys) <- get
            put ys
            return $ CoreLet [(y,x)] (CoreCase (CoreVar y) xs)
        g x = return x


-- Assume non-recursive lets
-- And entirely unique free variables
--
-- let a = b in (let c = d in ...) => let a = b ; c = d in ...
letMove :: Core -> Core
letMove = applyBodyCore (mapUnderCore f)
    where
        f (CoreLet bind1 (CoreLet bind2 x)) = coreLet (bind1++safe) (coreLet conflict x)
            where
                vars = map fst bind1
                (safe,conflict) = partition (isSafe . snd) bind2
                
                isSafe x = disjoint (collectFreeVars x) vars
                disjoint x y = length x == length (x \\ y)

        f x = x


-- let a = b in ... => f' b ...  ;  func f b ... = ...
letKill :: Core -> Core
letKill core = core{coreFuncs = concatMap f (coreFuncs core)}
    where
        f (CoreFunc name args body) = CoreFunc name args body2 : funcs
            where
                names = [name ++ "." ++ show i | i <- [1..]]
                (body2, (_, funcs)) = runState (mapUnderCoreM g body) (names, [])


        g :: CoreExpr -> State ([String], [CoreFunc]) CoreExpr
        g o@(CoreLet bind x) = do
            body <- g x
            (name:names,funcs) <- get
            let vars = collectFreeVars x \\ map fst bind
                newfunc = CoreFunc name (map fst bind ++ vars) body
            put (names,newfunc:funcs)
            return $ CoreApp (CoreFun name) (map snd bind ++ map CoreVar vars)

        g x = return x
