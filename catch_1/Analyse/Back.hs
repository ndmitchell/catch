
module Analyse.Back(backs) where

import Yhc.Core
import Analyse.Req
import Analyse.Info
import Data.Proposition
import Data.Maybe


type Property = CoreFuncName -> Constraint -> IO (PropReq Int)


backs :: Property -> PropReq CoreExpr -> IO (PropReq CoreVarName)
backs prop x = do
        info <- getInfo
        res <- fs info x
        return $ propChange (\(CoreVar i :< k) -> propLit $ i :< k) res
    where
        fs info = propMapM (f info)
        f  info (CoreVar x :< k) | isNothing (var info x) = return $ propLit $ CoreVar x :< k
        f  info x = back prop x >>= fs info



back :: Property -> Req CoreExpr -> IO (PropReq CoreExpr)
back prop (CoreVar x :< k) = do
        info <- getInfo
        let Just (on, c) = var info x
        return $ propLit $ on :< ((c |> k) info)

back prop (CoreApp (CoreCon c) xs :< k) = do
        info <- getInfo
        return $ replaceVars xs ((c <| k) info)

back prop (CoreCase on alts :< k) = do
        info <- getInfo
        return $ propAnds [f info (getCtor c) e | (c, e) <- alts]
    where
        getCtor (CoreCon c) = c
        getCtor (CoreApp x _) = getCtor x
        
        f info c e = propLit (on :< notin info c) `propOr` propLit (e :< k)

back prop (CoreApp (CoreFun f) xs :< k) = do
        c <- prop f k
        return $ replaceVars xs c
