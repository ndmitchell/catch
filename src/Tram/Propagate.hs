
module Tram.Propagate(propagate, collect) where

import Tram.Req
import Tram.Reduce
import Data.List
import General.General
import Data.Proposition
import Hill.All
import Safe


-- NOTE: may produce lots of (f,a) (f,b) pairs
-- can collapse some at the propagate stage if have same cond
-- which would give a speed up
propagate :: Hill -> Scope Formula -> Scopes Formula
propagate hill@(Hill _ funcs) (Scope func reqs) = res
    where
        res = scopesAnds [Scope name newReq |
                    (name,cond,Call _ args) <- collect hill isCall,
                    let newReq = cond `propOr` propMapReduce (g args) reqs,
                    not $ propIsTrue newReq]

        isCall (Call nam _) = nam == func
        isCall _ = False
        

        argList = funcArgs $ getFunc hill func
    
        g args (Req hill expr path ctor) = newReqs hill (mapOver (h args) expr) path ctor
        
        h args (Var name) = lookupJust name (zip argList args)
        h args x = x


collect :: Prop p => Hill -> (Expr -> Bool) -> [(FuncName, p Req, Expr)]
collect hill test = [(name,reqs,expr) | Func name _ body <- funcs hill, (reqs,expr) <- f propFalse body]
    where
        f prop expr = [(prop, expr) | test expr] ++
                      case expr of
                          Case on alts -> concatMap g alts
                              where
                                  allCtrs = ctorNames $ getCtor hill $ altCtr $ head alts
                                  seenCtrs = [x | AltCtr x _ <- alts]
                                  
                                  g (AltCtr ctr ex) = h (delete ctr allCtrs) ex
                                  g (Default ex) = h (allCtrs \\ seenCtrs) ex
                                  
                                  h ctrs ex = f (propOr prop reqs) ex
                                      where reqs = newReqs hill on (emptyPath hill) ctrs

                          
                          _ -> concatMap (f prop) (getChildren expr)
