
module Req(module Req, module Val) where

import Val
import Yhc.Core
import General
import Data.Proposition


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope CoreFuncName Vals

type Reqs = PropSimple Req

data Req = Req {reqExpr :: CoreExpr, reqVals :: Vals}
         deriving (Ord, Eq)


type ReqCall = (CoreFuncName, Vals)
-- ReqCall (name, vals) = Req (CoreApp (CoreFun name) [CoreVar "?"]) vals


instance Show Scope where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req expr x) = showCoreExprGroup expr ++ show x


instance PropLit Req where
    -- do not define anything, its a pretty bad PropLit!


-- precondition: all the Req's must be the same
collapse :: Core -> Reqs -> Vals
collapse core reqs
        | any (head lits /=) lits = error "Collapse, precondition violated"
        | otherwise = propFold fold reqs
    where
        fold = PropFold {foldOr = valsOrs core, foldAnd = valsAnds core
                        ,foldNot = error "collapse.foldNot", foldLit = reqVals}
        lits = map reqExpr $ propAll reqs
