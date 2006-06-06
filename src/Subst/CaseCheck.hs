
module Subst.CaseCheck(substCaseCheck) where

import Subst.Type
import Subst.Solve
import Subst.Show

import IO
import Hite
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap


substCaseCheck :: String -> Handle -> Hite -> IO Bool
substCaseCheck file hndl hite =
    do
        print base
        substSolve hite base
    where
        base = Env (IntMap.singleton 0 (SFunc "main" (replicate nArgs SFree))) (Map.singleton ("root!",[]) 0)
        nArgs = length $ funcArgs $ getFunc hite "main"

