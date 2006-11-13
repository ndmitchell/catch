
module Tram.Reduce(reduce, reduces, reduceWithM, reducesWithM, propMapReduceM, propMapReduce) where

import Tram.Type
import General.General
import Data.Proposition
import Data.BDD
import Control.Monad
import Control.Monad.Identity
import Hill.All
import Debug.Trace


reduces :: Reqs -> Reqs
reduces reqs = propMap reduce reqs


reduce :: Req -> Reqs
reduce req@(Req hite expr path ctors) = case expr of
	Call{} -> propLit req
	Var{} -> propLit req
	_ -> reduces $ reduceOne req


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Reqs
reduceOne req@(Req hite expr path ctors) = case expr of
	Star -> propFalse
	Sel x y -> newReqs hite x (path `integrate` y) ctors
	Make y xs -> propAnds (p1:ps)
		where
			cargs = ctorArgs $ getCtor hite y

			p1 = if ewpPath path then propBool (y `elem` ctors) else propTrue
			ps = zipWithEq f xs cargs
			
			f x carg = case path `differentiate` carg of
						   Nothing -> propTrue
						   Just path2 -> newReqs hite x path2 ctors
	
	_ -> error $ "reduceOne: " ++ show req
	


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: (Req -> IO Reqs) -> Reqs -> IO Reqs
reducesWithM f reqs = propMapReduceM (reduceWithM f) reqs


reduceWithM :: (Req -> IO Reqs) -> Req -> IO Reqs
reduceWithM f req@(Req hite expr path ctors) = case expr of
	Call{} -> f req >>= reducesWithM f
	Var{} -> return $ propLit req
	_ -> reducesWithM f $ reduceOne req


propMapReduceM :: Monad m => (Req -> m Reqs) -> Reqs -> m Reqs
propMapReduceM f x = mapBDDM (liftM reduces . f) x

propMapReduce :: (Req -> Reqs) -> Reqs -> Reqs
propMapReduce f x = runIdentity $ propMapReduceM (return . f) x
