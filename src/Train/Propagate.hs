
module Train.Propagate(propagate) where

import Train.Type
import Data.Maybe
import General.General
import Data.BDD


propagate :: ZHite -> Scope -> Scopes
propagate (ZHite _ funcs) (Scope func reqs) = res
	where
		res = bddAnds $ map bddLit $ concatMap f funcs
		funcArgs = head [args | ZFunc name args _ <- funcs, name == func]
	
		--f (ZFunc name _ xs) = [error $ output reqs ++ " --> " ++ output (mapBDD (g args) reqs) |
		--	(cond, Right code) <- xs, ZCall calls args <- allOver code, calls == func]
		f (ZFunc name _ xs) = [Scope name newReq |
			(cond, Right code) <- xs, ZCall calls args <- allOver code, calls == func,
			let newReq = reqsNot cond `bddOr` mapBDD (g args) reqs, not $ bddIsTrue newReq]
			
		g args (Req hite expr path ctor) = bddLit $ newReq hite (mapOver (h args) expr) path ctor
		
		h args (ZVar name) = lookupJust name (zip funcArgs args)
		h args x = x
