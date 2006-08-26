
module Train.Propagate(propagate) where

import Train.Type
import Data.Maybe
import General.General
import Data.Predicate


propagate :: ZHite -> Scope -> Scopes
propagate (ZHite _ funcs) (Scope func reqs) = predAnd $ map predLit $ concatMap f funcs
	where
		funcArgs = head [args | ZFunc name args _ <- funcs, name == func]
	
		f (ZFunc name _ xs) = [Scope name (predOr [predNot cond, mapPredLit (g args) reqs]) |
			(cond, Right code) <- xs, ZCall calls args <- allOver code, calls == func]
			
		g args (Req hite expr path ctor) = predLit $ Req hite (mapOver (h args) expr) path ctor
		
		h args (ZVar name) = lookupJust name (zip funcArgs args)
		h args x = x
