
module Analyse.Precond(precond) where

import Yhc.Core
import Data.Proposition
import Control.Monad
import qualified Data.Map as Map

import Analyse.Req
import Analyse.Info
import Analyse.Property
import Analyse.Back
import Analyse.Fix


-- given a list of all functions, return the constraint on "main"
precond :: (String -> IO ()) -> [CoreFuncName] -> IO Constraint
precond logger funcs = do
        info <- getInfo
        res <- fix logger conTrue conAnd (compute info) (Map.fromList [(k,conTrue) | k <- funcs])
        res <- return $ Map.filter (/= conTrue) res
        logger ""
        logger "FINAL PRECONDITIONS"
        when (Map.null res) $ logger "    None, program is safe"
        loggerMap res
        return $ Map.findWithDefault conTrue "main" res
    where
        loggerLine k v = logger $ "    " ++ show k ++ " = " ++ show v
        loggerMap = mapM (uncurry loggerLine) . Map.toList
        
        compute :: Info -> (CoreFuncName -> IO Constraint) -> CoreFuncName -> IO Constraint
        compute info ask func = do
            let CoreFunc _ [arg] body = function info func
                get name = ask name >>= return . propLit . (0 :<)
            res <- pre get body
            res <- backs property res
            return $ propCon arg res




pre :: (CoreFuncName -> IO (PropReq Int)) -> CoreExpr -> IO (PropReq CoreExpr)
pre preFunc (CoreVar x) = return propTrue
pre preFunc (CoreApp (CorePrim "error") _) = return propFalse

pre preFunc (CoreApp (CoreCon _) xs) = liftM propAnds $ mapM (pre preFunc) xs

pre preFunc (CoreApp (CoreFun f) xs) = do
    p <- preFunc f
    xs2 <- mapM (pre preFunc) xs
    return $ propAnds $ replaceVars xs p : xs2

pre preFunc (CoreCase on alts) = do
        info <- getInfo
        alts <- coreAlts alts
        on2 <- pre preFunc on
        alts2 <- mapM (f info) alts
        return $ propAnds $ on2 : alts2
    where
        f info (c,e) = do
            x <- pre preFunc e
            return $ propOr (propLit $ on :< notin info c) x

