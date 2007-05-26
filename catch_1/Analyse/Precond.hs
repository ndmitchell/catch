
module Analyse.Precond(preconds, precond) where

import Yhc.Core
import Data.Proposition
import Control.Monad
import qualified Data.Map as Map

import Analyse.Req
import Analyse.Info
import Analyse.Property
import Analyse.Back
import Analyse.Fix



preconds :: (String -> IO ()) -> Bool -> [String] -> [CoreFuncName] -> IO Constraint
preconds logger partials errmsgs funcs = do
        cs <- zipWithM f [1..] errmsgs
        info <- getInfo
        return $ conAnds info cs
    where
        f n msg = do
            putStrLn $ "Checking [" ++ show n ++ "/" ++ show (length errmsgs) ++ "]: " ++ msg
            res <- precond logger partials [n] funcs
            putStrLn $ "Answer: " ++ show res
            return res


-- given a list of all functions, return the constraint on "main"
-- first argument logs stuff
-- second argument says which error messages should fail (True == error)
precond :: (String -> IO ()) -> Bool -> [Int] -> [CoreFuncName] -> IO Constraint
precond logger partials errcheck funcs = do
        info <- getInfo
        let true = conTrue info
        res <- fix logger partCheck true conAnd (compute info) (Map.fromList [(k,true) | k <- funcs])
        res <- return $ Map.filter (/= true) res
        logger ""
        logger "FINAL PRECONDITIONS"
        when (Map.null res) $ logger "    None, program is safe"
        loggerMap res
        return $ Map.findWithDefault true "main" res
    where
        partCheck x = partials && conBool x /= Just True
    
        loggerLine k v = logger $ "    " ++ show k ++ " = " ++ show v
        loggerMap = mapM (uncurry loggerLine) . Map.toList
        
        compute :: Info -> (CoreFuncName -> IO Constraint) -> CoreFuncName -> IO Constraint
        compute info ask func = do
            let CoreFunc _ [arg] body = function info func
                get name = ask name >>= return . propLit . (0 :<)
            res <- pre errcheck get body
            res <- backs property res
            return $ propCon info arg res




pre :: [Int] -> (CoreFuncName -> IO (PropReq Int)) -> CoreExpr -> IO (PropReq CoreExpr)
pre errcheck preFunc x = f x
    where
        f x | isCoreVar x || isCoreConst x = return propTrue
        
        f (CoreApp (CoreCon _) xs) = liftM propAnds $ mapM f xs
        f (CoreApp (CoreVar _) xs) = liftM propAnds $ mapM f xs

        f (CoreFun fn) = f $ CoreApp (CoreFun fn) []
        f (CoreApp (CoreFun "Prelude.error") [CoreInt i]) | i `elem` errcheck = return $ propFalse
        f (CoreApp (CoreFun fn) xs) = do
            info <- getInfo
            if isCorePrim (function info fn)
                then liftM propAnds $ mapM f xs
                else do
                    p <- preFunc fn
                    xs2 <- mapM (pre errcheck preFunc) xs
                    return $ propAnds $ replaceVars xs p : xs2

        f (CoreCase on alts) = do
            info <- getInfo
            alts <- coreAlts alts
            on2 <- f on
            alts2 <- mapM (g info) alts
            return $ propAnds $ on2 : alts2
            where
            g info (c,e) = do
                x <- f e
                return $ propOr (propLit $ on :< notin info c) x

        f (CoreLam vars x) = f $ replaceFreeVars (zip vars $ repeat (CoreFun "any?")) x

        f x = error $ "Analyse.Precond.pre, unhandled: " ++ show x
