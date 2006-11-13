
module Tram.Propagate(propagate, collect) where

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


collect :: Hill -> (Expr -> Bool) -> [(FuncName, Reqs, Expr)]
collect hill test = [(name,reqs,expr) | Func name _ body <- funcs hill, (reqs,expr) <- f propTrue body]
    where
        f prop expr = [(prop, expr) | test expr] ++
                      case expr of
                          Case on alts -> concatMap (g prop expr) alts
                          _ -> concatMap (f prop) (getChildren expr)

        g prop (Case on alts) (Default expr) = f (propAnd prop req) expr
            where req = newReqs hill on (emptyPath hill) (ctorOthers $ getCtor hill $ altCtr $ head alts)
        
        g prop (Case on alts) (AltCtr ctr expr) = f (propAnd prop req) expr
            where req = newReqs hill on (emptyPath hill) [ctr]
