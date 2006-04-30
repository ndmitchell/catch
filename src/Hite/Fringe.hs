{-
    A fringe is the edge of a function, which is called by other functions
-}


module Hite.Fringe(Fringe, increaseFringe, calcFringe) where

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

