
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
        f  info x = back info prop x >>= fs info



back :: Info -> Property -> Req CoreExpr -> IO (PropReq CoreExpr)
back info prop (_ :< k) | isJust kb = return $ propBool $ fromJust kb
    where kb = conBool k

back info prop (CoreVar x :< k) =
        return $ propLit $ on :< (c |> k)
    where
        Just (on, c) = var info x

back info prop (CoreApp (CoreCon c) xs :< k) =
        return $ replaceVars xs (c <| k)

back info prop (CoreCase on alts :< k) = do
        alts <- coreAlts alts
        return $ propAnds [f c e | (c, e) <- alts]
    where
        f c e = propLit (on :< notin info c) `propOr` propLit (e :< k)

back info prop (CoreFun f :< k) = back info prop (CoreApp (CoreFun f) [] :< k)
back info prop (CoreApp (CoreFun f) xs :< k)
    | f == "Prelude.error" = return propTrue
    | isCorePrim (function info f) = return propFalse
    | otherwise = do
        c <- prop f k
        return $ replaceVars xs c

back info prop (CoreStr x :< k) = back info prop (explode x :< k)
    where
        explode [] = CoreApp (CoreCon "[]") []
        explode (x:xs) = CoreApp (CoreCon ":") [CoreChr x, CoreStr xs]

back info prop x = error $ "Unhandled back, " ++ show x
