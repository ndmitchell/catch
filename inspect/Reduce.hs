
module Reduce(reduce, reduces, reduceWithM, reducesWithM, propMapReduceM, propMapReduce) where

import Req
import General
import DataRep
import Data.Proposition
import Control.Monad
import Control.Monad.Identity
import Yhc.Core
import Data.List
import Data.Maybe


-- DRIVERS

reduces :: Core -> Reqs -> Reqs
reduces core reqs = propMap (reduce core) reqs

reduce :: Core -> Req -> Reqs
reduce core req@(Req expr _) = case expr of
    CoreApp (CoreFun x) _ | not $ "." `isPrefixOf` x -> propLit req
    CoreVar x -> propLit req
    _ -> reduces core $ reduceOne core req
reduce core x = propLit x


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: Core -> (Req -> IO Reqs) -> Reqs -> IO Reqs
reducesWithM core f reqs = propMapReduceM core (reduceWithM core f) reqs


reduceWithM :: Core -> (Req -> IO Reqs) -> Req -> IO Reqs
reduceWithM core f req@(Req expr _) = case expr of
    CoreApp (CoreFun x) _ | not $ "." `isPrefixOf` x -> f req >>= reducesWithM core f
    CoreVar x -> return $ propLit req
    _ -> reducesWithM core f $ reduceOne core req
reduceWithM core f x = return $ propLit x


propMapReduceM :: Monad m => Core -> (Req -> m Reqs) -> Reqs -> m Reqs
propMapReduceM core f x = propMapM (liftM (reduces core) . f) x

propMapReduce :: Core -> (Req -> Reqs) -> Reqs -> Reqs
propMapReduce core f x = runIdentity $ propMapReduceM core (return . f) x




-- CORE LOGIC

-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Core -> Req -> Reqs
reduceOne core req@(Req expr vals) = case expr of
    CoreApp (CoreFun ('.':y)) [x] -> propLit $ Req x (integrate core vals y)

    CoreApp (CoreCon y) xs -> propAnds (p1:ps)
        where
            cargs = map (fromJust . snd) $ coreCtorFields $ coreCtor core y

            p1 = propBool $ checkRoot core vals y
            ps = zipWithEq f xs cargs
            
            f x carg = propLit $ Req x (differentiate core vals carg)

    CoreCase on alts -> propAnds $ map f alts
        where
            allCtrs = ctorNames core $ fromCoreCon $ fst $ head alts
            seenCtrs = [x | (CoreCon x, _) <- alts]

            f (CoreCon ctr, rhs) = g (delete ctr allCtrs) rhs
            f (CoreVar _, rhs) = g seenCtrs rhs
            
            g ctrs ex = propLit (Req on $ anyCtor core ctrs) `propOr` propLit (Req ex vals)

    CoreApp (CorePrim "error") _ -> propLit Angelic -- since will never return anything
    CoreApp (CorePrim x) ys -> propLit Demonic -- absolutely no idea what the result is
    c | isCoreConst c -> propLit Demonic -- if you care, abstract before here

    _ -> error $ "reduceOne: " ++ show req

