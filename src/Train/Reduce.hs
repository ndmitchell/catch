
module Train.Reduce(reduce, reduces, reduceWithM, reducesWithM) where

import Train.Type
import General.General
import Data.BDD
import Hite


reduces :: Reqs -> Reqs
reduces reqs = mapBDD reduce reqs


reduce :: Req -> Reqs
reduce req@(Req hite expr path ctors) = case expr of
	ZCall{} -> bddLit req
	ZVar{} -> bddLit req
	_ -> reduces $ reduceOne req


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Reqs
reduceOne req@(Req hite expr path ctors) = case expr of
	ZSel x y -> bddLit $ newReq hite x (path `integrate` y) ctors
	ZMake y xs -> bddAnds (p1:ps)
		where
			cargs = ctorArgs $ getCtor hite y

			p1 = if ewpPath path then bddBool (y `elem` ctors) else bddTrue
			ps = zipWithEq f xs cargs
			
			f x carg = case path `differentiate` carg of
						   Nothing -> bddTrue
						   Just path2 -> bddLit $ newReq hite x path2 ctors
	
	_ -> error $ "reduceOne: " ++ output req
	


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: (Req -> IO Reqs) -> Reqs -> IO Reqs
reducesWithM f reqs = mapBDDM (reduceWithM f) reqs


reduceWithM :: (Req -> IO Reqs) -> Req -> IO Reqs
reduceWithM f req@(Req hite expr path ctors) = case expr of
	ZCall{} -> f req >>= reducesWithM f
	ZVar{} -> return $ bddLit req
	_ -> reducesWithM f $ reduceOne req
	