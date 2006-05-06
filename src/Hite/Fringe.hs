{-
    A fringe is the edge of a function, which is called by other functions
-}


module Hite.Fringe(Fringe, increaseFringe, calcFringe, annotateFringe) where

import Hite.Type
import Hite.Reachable


type Fringe = [FuncName]


increaseFringe :: Hite -> Fringe -> [Fringe]
increaseFringe hite fringe = map (:[])
    [name |
        Func name _ body _ <- funcs hite,
        let calls = [x | CallFunc x <- allExpr body],
        any (`elem` fringe) calls]


calcFringe :: Hite -> Fringe -> Hite
calcFringe hite fringe = reachableList fringe hite



annotateFringe :: Fringe -> Hite -> Hite
annotateFringe fringe hite = hite{funcs = concatMap f (funcs hite)}
    where
        f func@(Func name args body pos) | name `elem` fringe =
            [func, Func ('!':name) args (MCase [MCaseAlt (MCaseAnd []) $ Call (CallFunc name) (map Var args)]) pos]
        f x = [x]

