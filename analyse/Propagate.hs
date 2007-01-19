
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
propagate :: Core -> Scope -> Scopes
propagate hill (Scope func reqs) 
        | null (vars \\ argList) = res -- standard propagate
        | otherwise = [Scope func $ reqs] -- propagate only to the lets
    where
        vars = nub $ map (fromCoreVar . reqExpr) $ propAll reqs
    
        res = scopesAnds [Scope name newReq |
                    (name,cond,CoreApp (CoreFun _) args) <- collect hill isCall,
                    let newReq = cond `propOr` propMapReduce (g args) reqs,
                    not $ propIsTrue newReq]

        isCall (CoreApp (CoreFun nam) _) = nam == func
        isCall _ = False
        
        CoreFunc _ argList funcBody = coreFunc hill func
        
    
        g args (Req expr (PathCtor hill path ctor)) = newReqs hill (mapUnderCore (h args) expr) path ctor
        
        h args (CoreVar name) = lookupJustNote "Tram.Propagate.propagate"  name (zip argList args)
        h args x = x


collect :: Core -> (CoreExpr -> Bool) -> [(CoreFuncName, Reqs, CoreExpr)]
collect hill test = [(name,reqs,expr) | CoreFunc name _ body <- coreFuncs hill, (reqs,expr) <- f propFalse body]
    where
        f prop expr =
            [(prop, expr) | test expr] ++
            case expr of
                CoreCase (CoreVar on) alts -> concatMap g alts
                    where
                        allCtrs = ctorNames hill $ fromCoreCon $ fst $ head alts
                        seenCtrs = [x | (CoreCon x, _) <- alts]

                        g (CoreCon ctr, rhs) = h (delete ctr allCtrs) rhs
                        g (CoreVar _, rhs) = h seenCtrs rhs

                        h ctrs ex = f (propOr prop reqs) ex
                            where reqs = newReqs hill (CoreVar on) (emptyPath hill) ctrs

                _ -> concatMap (f prop) (getChildrenCore expr)
