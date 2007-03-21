
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



preconds :: (String -> IO ()) -> [String] -> [CoreFuncName] -> IO Constraint
preconds logger errmsgs funcs = do
        cs <- zipWithM f [0..] errmsgs
        info <- getInfo
        return $ conAnds info cs
    where
        f n msg = do
            putStrLn $ "Checking: " ++ msg
            res <- precond logger (g n) funcs
            putStrLn $ "Answer: " ++ show res
            return res

        g n (CoreInt i) = n == i



-- given a list of all functions, return the constraint on "main"
-- first argument logs stuff
-- second argument says if an error should fail (True == error)
precond :: (String -> IO ()) -> (CoreExpr -> Bool) -> [CoreFuncName] -> IO Constraint
precond logger errcheck funcs = do
        info <- getInfo
        let true = conTrue info
        res <- fix logger true conAnd (compute info) (Map.fromList [(k,true) | k <- funcs])
        res <- return $ Map.filter (/= true) res
        logger ""
        logger "FINAL PRECONDITIONS"
        when (Map.null res) $ logger "    None, program is safe"
        loggerMap res
        return $ Map.findWithDefault true "main" res
    where
        loggerLine k v = logger $ "    " ++ show k ++ " = " ++ show v
        loggerMap = mapM (uncurry loggerLine) . Map.toList
        
        compute :: Info -> (CoreFuncName -> IO Constraint) -> CoreFuncName -> IO Constraint
        compute info ask func = do
            let CoreFunc _ [arg] body = function info func
                get name = ask name >>= return . propLit . (0 :<)
            res <- pre errcheck get body
            res <- backs property res
            return $ propCon info arg res




pre :: (CoreExpr -> Bool) -> (CoreFuncName -> IO (PropReq Int)) -> CoreExpr -> IO (PropReq CoreExpr)
pre errcheck preFunc x = f x
    where
        f x | isCoreVar x || isCoreConst x = return propTrue
        
        f (CoreApp (CorePrim prim) xs)
            | prim == "Prelude.error" = return $ propBool $ not $ errcheck $ head xs
            | otherwise = liftM propAnds $ mapM f xs

        f (CorePrim prim) = return propTrue

        f (CoreApp (CoreCon _) xs) = liftM propAnds $ mapM f xs
        f (CoreApp (CoreVar _) xs) = liftM propAnds $ mapM f xs

        f (CoreApp (CoreFun fn) xs) = do
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

        f x = error $ "Analyse.Precond.pre, unhandled: " ++ show x
