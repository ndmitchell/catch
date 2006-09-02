
module Train.Propagate(propagate) where

import Train.Type
import Data.Maybe
import General.General
import Data.BDD


propagate :: ZHite -> Scope -> Scopes
propagate zhite@(ZHite _ funcs) (Scope func reqs) = res
	where
		res = bddAnds $ map bddLit $ concatMap f funcs
		ZFunc _ funcArgs _ = getZFunc zhite func
	
		f (ZFunc name _ xs) = [Scope name newReq |
			(cond, Right code) <- xs, ZCall calls args <- allOver code, calls == func,
			let newReq = reqsNot cond `bddOr` mapBDD (g args) reqs, not $ bddIsTrue newReq]
			
		g args (Req hite expr path ctor) = newReqs hite (mapOver (h args) expr) path ctor
		
		h args (ZVar name) = lookupJust name (zip funcArgs args)
		h args x = x
