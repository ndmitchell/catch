

module Checker.Propagate(propagate) where

import Hite
import Checker.Backward
import Constraint

import Maybe
import General.General



propagate :: Hite -> FuncName -> Reqs -> ReqAlls
propagate hite on reqs
    | isTrue reqs = predTrue
    | otherwise = predAnd $ res
        where
            orig = funcArgs $ getFunc hite on

            f rep (Req (Var var) path set h) = predLit $ Req (fromJust $ lookup var rep) path set h

            res = [predLit $ ReqAll (funcName func) $
                    predOr [reqsNot hite $ mcasePred hite cond, mapPredLit (f $ zip orig args) reqs] |
                    func <- funcs hite, let MCase alts = body func, MCaseAlt cond act <- alts,
                    (Call (CallFunc n) args) <- allExpr act, n == on]

