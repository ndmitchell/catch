
module Tram.Reduce(reduce, reduces, reduceWithM, reducesWithM, propMapReduceM, propMapReduce) where

import Tram.Req
import General.General
import Data.Proposition
import Control.Monad
import Control.Monad.Identity
import Hill.All
import Data.List


-- | Should have no negate done first
reduces :: Prop p => p Req -> p Req
reduces reqs = propMap reduce reqs


reduce :: Prop p => Req -> p Req
reduce req@(Req hill expr path ctors) = case expr of
    Call{} -> propLit req
    Var{} -> propLit req
    _ -> reduces $ reduceOne req


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Prop p => Req -> p Req
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
            f (Default ex) = g (allCtrs \\ seenCtrs) ex
            
            g ctrs ex = newReqs hill on (emptyPath hill) ctrs `propOr` newReqs hill ex path ctors

    _ -> error $ "reduceOne: " ++ show req
    


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: Prop p => (Req -> IO (p Req)) -> p Req -> IO (p Req)
reducesWithM f reqs = propMapReduceM (reduceWithM f) reqs


reduceWithM :: Prop p => (Req -> IO (p Req)) -> Req -> IO (p Req)
reduceWithM f req@(Req hill expr path ctors) = case expr of
    Call{} -> f req >>= reducesWithM f
    Var{} -> return $ propLit req
    _ -> reducesWithM f $ reduceOne req


propMapReduceM :: (Prop p, Monad m) => (Req -> m (p Req)) -> p Req -> m (p Req)
propMapReduceM f x = propMapM (liftM reduces . f) x

propMapReduce :: Prop p => (Req -> p Req) -> p Req -> p Req
propMapReduce f x = runIdentity $ propMapReduceM (return . f) x
