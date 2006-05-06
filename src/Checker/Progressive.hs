{- |
    Solve the question "progressively"
    Start with the minimal fragment, progressively increase it
    The idea is the minimal fragment will fixed point quickly
    Saving _loads_ of time from the rest.
-}

module Checker.Progressive(progressiveSolve) where


import Hite
import Constraint
import Checker.Solver
import General.Output



progressiveSolve :: Hite -> Reqs -> OutputMonad Reqs
progressiveSolve hite reqs = solve hite2 reqs
    where
        hite2 = annotateFringe ["main"] hite
