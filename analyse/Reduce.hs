
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


-- | Should have no negate done first
reduces :: Reqs -> Reqs
reduces reqs = propMap reduce reqs


reduce :: Req -> Reqs
reduce req@(Req expr (PathCtor hill path ctors)) = case expr of
    CoreApp (CoreFun x) _ | not $ "." `isPrefixOf` x -> propLit req
    CoreVar x -> propLit req
    _ -> reduces $ reduceOne req
reduce x = propLit x


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Reqs
reduceOne req@(Req expr (PathCtor hill path ctors)) = case expr of
    CoreApp (CoreFun ('.':y)) [x] -> newReqs hill x (path `integrate` y) ctors
    
    CoreApp (CoreCon y) xs -> propAnds (p1:ps)
        where
            cargs = map (fromJust . snd) $ coreCtorFields $ coreCtor hill y

            p1 = if ewpPath path then propBool (y `elem` ctors) else propTrue
            ps = zipWithEq f xs cargs
            
            f x carg = case path `differentiate` carg of
                           Nothing -> propTrue
                           Just path2 -> newReqs hill x path2 ctors
    
    CoreCase on alts -> propAnds $ map f alts
        where
            allCtrs = ctorNames $ coreCtorData hill $ fromCoreCon $ fst $ head alts
            seenCtrs = [x | (CoreCon x, _) <- alts]

            f (CoreCon ctr, rhs) = g (delete ctr allCtrs) rhs
            f (CoreVar _, rhs) = g seenCtrs rhs
            
            g ctrs ex = newReqs hill on (emptyPath hill) ctrs `propOr` newReqs hill ex path ctors

    CoreApp (CorePrim "error") _ -> propLit Angelic -- since will never return anything
    CoreApp (CorePrim x) ys -> propLit Demonic -- absolutely no idea what the result is
    c | isCoreConst c -> propLit Demonic -- if you care, abstract before here

    _ -> error $ "reduceOne: " ++ show req
    


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: (Req -> IO Reqs) -> Reqs -> IO Reqs
reducesWithM f reqs = propMapReduceM (reduceWithM f) reqs


reduceWithM :: (Req -> IO Reqs) -> Req -> IO Reqs
reduceWithM f req@(Req expr (PathCtor hill path ctors)) = case expr of
    CoreApp (CoreFun _) _ -> f req >>= reducesWithM f
    CoreVar x -> return $ propLit req
    _ -> reducesWithM f $ reduceOne req
reduceWithM f x = return $ propLit x


propMapReduceM :: Monad m => (Req -> m Reqs) -> Reqs -> m Reqs
propMapReduceM f x = propMapM (liftM reduces . f) x

propMapReduce :: (Req -> Reqs) -> Reqs -> Reqs
propMapReduce f x = runIdentity $ propMapReduceM (return . f) x



splitVar :: String -> (String, [String])
splitVar xs = (y, ys)
    where (y:ys) = words [if x == '.' then ' ' else x | x <- xs]
