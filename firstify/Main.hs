
module Main where

import System.FilePath
import Yhc.Core
import System.Environment
import Data.Maybe
import Data.List


main = do
        x <- getArgs
        mapM_ f x
    where
        f file = do
            core <- loadCore file
            let file2 = replaceBaseName file (takeBaseName file ++ "_first")
                (fo,core2) = firstify $ mapUnderCore remCorePos $ lambdas $ zeroApp core
            saveCore file2 core2
            print core2
            putStrLn $ "-- " ++ (if fo then "FIRST" else "HIGHER") ++ " order"


lambdas :: Core -> Core
lambdas core = mapUnderCore f core
    where
        f orig@(CoreApp (CoreFun name) args) | extra > 0 =
                CoreLam new (CoreApp (CoreFun name) (args ++ map CoreVar new))
            where
                extra = arity core name - length args
                new = take extra $ ['v':show i | i <- [1..]] \\ collectAllVars orig
        f x = x


zeroApp :: Core -> Core
zeroApp = mapUnderCore f
    where
        f (CoreFun x) = CoreApp (CoreFun x) []
        f (CoreApp (CoreApp x ys) zs) = CoreApp x (ys++zs)
        f x = x



firstify :: Core -> (Bool, Core)
firstify x = f 10 x
    where
        f n x | not $ hasHO x = (True, x)
              | n == 0 = (False, x)
              | otherwise = f (n-1) (process x)
            where process = coreReachable ["main"] . coreSimplify . specHO . coreSimplify . inlineHO


arity :: Core -> String -> Int
arity core name = length $ coreFuncArgs $ coreFunc core name


hasHO :: Core -> Bool
hasHO core = any isCoreLam (allCore core)


isHO :: Core -> CoreExpr -> Bool
isHO core (CoreLet _ x) = isHO core x
isHO core (CoreLam _ _) = True
isHO core (CoreCase x ys) = any (isHO core . snd) ys
isHO core (CoreApp (CoreCon _) args) = any (isHO core) args
isHO core _ = False


inlineHO :: Core -> Core
inlineHO core = mapUnderCore f $ zeroApp core
    where
        inline = [(coreFuncName func, func) | func <- coreFuncs core, isHO core $ coreFuncBody func]
        
        f (CoreApp (CoreFun name) args) | isJust func = coreLam extra rest
            where
                func = lookup name inline
                (extra,rest) = coreInlineFuncLambda (fromJust func) args

        f x = x




specHO :: Core -> Core
specHO = id