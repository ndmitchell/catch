{-
    A fringe is the edge of a function, which is called by other functions
-}


module Hite.Fringe(Fringe, increaseFringe, calcFringe, annotateFringe) where

import Hite.Type
import Hite.Reachable
import List


type Fringe = [FuncName]


increaseFringe :: Hite -> Fringe -> Fringe
increaseFringe hite fringe = newfringe
    where
        newfringe = nub [x |
                Func name _ body _ <- funcs hite,
                not (name `elem` newset),
                CallFunc x <- allExpr body,
                x `elem` newset]
    
        newset = calcNames callers
        existing = calcNames fringe
        
        -- those functions that call one of the functions in the old fringe
        callers = [name |
            Func name _ body _ <- funcs hite,
            not (name `elem` existing),
            let calls = [x | CallFunc x <- allExpr body],
            any (`elem` fringe) calls]
        
        
        calcNames x = map funcName $ funcs $ reachableList x hite


calcFringe :: Hite -> Fringe -> Hite
calcFringe hite fringe = reachableList fringe hite



annotateFringe :: Fringe -> Hite -> Hite
annotateFringe fringe hite = hite{funcs = concatMap f (funcs hite)}
    where
        f func@(Func name args body pos) | name `elem` fringe =
            [func, Func ('!':name) args (blankMCase $ Call (CallFunc name) (map Var args)) pos]
        f x = [x]

