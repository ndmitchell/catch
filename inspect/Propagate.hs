
module Propagate(propagate, collect) where

import Req
import Reduce
import Data.List
import Data.Maybe
import General
import DataRep
import Data.Proposition
import Yhc.Core
import Safe


-- NOTE: may produce lots of (f,a) (f,b) pairs
-- can collapse some at the propagate stage if have same cond
-- which would give a speed up
propagate :: Core -> Scope -> [(CoreFuncName, Reqs)]
propagate core (Scope func vals) = res
    where
        res = merge [(name,newReq) |
                    (name,cond,CoreApp (CoreFun _) [arg]) <- collect core isCall,
                    let newReq = cond `propOr` propLit (Req arg vals)]

        isCall (CoreApp (CoreFun nam) _) = nam == func
        isCall _ = False
        
        merge xs = [(fst $ head x, propAnds $ map snd x) | x <- groupSetExtract fst xs]


collect :: Core -> (CoreExpr -> Bool) -> [(CoreFuncName, Reqs, CoreExpr)]
collect core test = [(name,reqs,expr) | CoreFunc name _ body <- coreFuncs core, (reqs,expr) <- f propFalse body]
    where
        f prop expr =
            [(prop, expr) | test expr] ++
            case expr of
                CoreCase (CoreVar on) alts -> concatMap g alts
                    where
                        allCtrs = ctorNames core $ fromCoreCon $ fst $ head alts
                        seenCtrs = [x | (CoreCon x, _) <- alts]

                        g (CoreCon ctr, rhs) = h (delete ctr allCtrs) rhs
                        g (CoreVar _, rhs) = h seenCtrs rhs

                        h ctrs ex = f (propOr prop reqs) ex
                            where reqs = propLit $ Req (CoreVar on) (anyCtor core ctrs)

                _ -> concatMap (f prop) (getChildrenCore expr)
