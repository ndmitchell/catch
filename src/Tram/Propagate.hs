
module Tram.Propagate(propagate, collect) where

import Tram.Type
import Tram.Reduce
import Data.Maybe
import General.General
import Data.Proposition
import Hill.All
import Safe

-- NOTE: may produce lots of (f,a) (f,b) pairs
-- can collapse some at the propagate stage if have same cond
-- which would give a speed up
propagate :: Hill -> Scope -> Scopes
propagate hill@(Hill _ funcs) (Scope func reqs) = res
    where
        res = scopesAnds [Scope name newReq |
                    (name,cond,Call _ args) <- collect hill isCall,
                    let newReq = reqsNot cond `propOr` propMapReduce (g args) reqs,
                    not $ propIsTrue newReq]

        isCall (Call nam _) = nam == func
        isCall _ = False
        

        argList = funcArgs $ getFunc hill func
    
        g args (Req hill expr path ctor) = newReqs hill (mapOver (h args) expr) path ctor
        
        h args (Var name) = lookupJust name (zip argList args)
        h args x = x


collect :: Hill -> (Expr -> Bool) -> [(FuncName, Reqs, Expr)]
collect hill test = [(name,reqs,expr) | Func name _ body <- funcs hill, (reqs,expr) <- f propTrue body]
    where
        f prop expr = [(prop, expr) | test expr] ++
                      case expr of
                          Case on alts -> concatMap (g prop expr) alts
                          _ -> concatMap (f prop) (getChildren expr)

        g prop (Case on alts) (Default expr) = f (propAnd prop req) expr
            where req = newReqs hill on (emptyPath hill) (defaultAlts hill alts)
        
        g prop (Case on alts) (AltCtr ctr expr) = f (propAnd prop req) expr
            where req = newReqs hill on (emptyPath hill) [ctr]
