
module Tram.Propagate(propagate) where

import Tram.Type
import Tram.Reduce
import Data.Maybe
import General.General
import Data.Proposition
import Hill.All

-- NOTE: may produce lots of (f,a) (f,b) pairs
-- can collapse some at the propagate stage if have same cond
-- which would give a speed up
propagate :: Hill -> Scope -> Scopes
propagate hill@(Hill _ funcs) (Scope func reqs) = res
	where
		res = scopesAnds $ concatMap f funcs
		Func _ funcArgs _ = getFunc hill func
	
		f (Func name _ xs) = error "Propagate.propagate, todo" {- [Scope name newReq |
			(cond, Right code) <- xs, Call calls args <- allOver code, calls == func,
			let newReq = reqsNot cond `propOr` propMapReduce (g args) reqs, not $ propIsTrue newReq] -}
			
		g args (Req hite expr path ctor) = newReqs hite (mapOver (h args) expr) path ctor
		
		h args (Var name) = lookupJust name (zip funcArgs args)
		h args x = x
