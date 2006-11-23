
module Tram.Reduce(reduce, reduces, reduceWithM, reducesWithM, propMapReduceM, propMapReduce) where

import Tram.Req
import General.General
import Data.Proposition
import Control.Monad
import Control.Monad.Identity
import Hill.All
import Data.List


-- | Should have no negate done first
reduces :: Formula Req -> Formula Req
reduces reqs = propMap reduce reqs


reduce :: Req -> Formula Req
reduce req@(Req hill expr path ctors) = case expr of
    Call{} -> propLit req
    Var{} -> propLit req
    _ -> reduces $ reduceOne req
reduce x = propLit x


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Formula Req
reduceOne req@(Req hill expr path ctors) = case expr of
    Star -> propFalse
    Sel x y -> newReqs hill x (path `integrate` y) ctors
    Make y xs -> propAnds (p1:ps)
        where
            cargs = ctorArgs $ getCtor hill y

            p1 = if ewpPath path then propBool (y `elem` ctors) else propTrue
            ps = zipWithEq f xs cargs
            
            f x carg = case path `differentiate` carg of
                           Nothing -> propTrue
                           Just path2 -> newReqs hill x path2 ctors
    
    Case on alts -> propAnds $ map f alts
        where
            allCtrs = ctorNames $ getCtor hill $ altCtr $ head alts
            seenCtrs = [x | AltCtr x _ <- alts]

            f (AltCtr ctr ex) = g (delete ctr allCtrs) ex
            f (Default ex) = g seenCtrs ex
            
            g ctrs ex = newReqs hill on (emptyPath hill) ctrs `propOr` newReqs hill ex path ctors

    Let binds x -> newReqs hill x path ctors

    Prim x ys -> propLit Demonic -- absolutely no idea what the result is
    Const _ -> propLit Demonic -- if you care, abstract before here
    Error _ -> propLit Angelic -- since will never return anything

    _ -> error $ "reduceOne: " ++ show req
    


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: (Req -> IO (Formula Req)) -> Formula Req -> IO (Formula Req)
reducesWithM f reqs = propMapReduceM (reduceWithM f) reqs


reduceWithM :: (Req -> IO (Formula Req)) -> Req -> IO (Formula Req)
reduceWithM f req@(Req hill expr path ctors) = case expr of
    Call{} -> f req >>= reducesWithM f
    Var{} -> return $ propLit req
    _ -> reducesWithM f $ reduceOne req
reduceWithM f x = return $ propLit x


propMapReduceM :: Monad m => (Req -> m (Formula Req)) -> Formula Req -> m (Formula Req)
propMapReduceM f x = propMapM (liftM reduces . f) x

propMapReduce :: (Req -> Formula Req) -> Formula Req -> Formula Req
propMapReduce f x = runIdentity $ propMapReduceM (return . f) x
