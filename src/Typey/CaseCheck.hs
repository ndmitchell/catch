
module Typey.CaseCheck(typeyCaseCheck, typeyHoCaseCheck, abstractCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.LiftData
import Typey.Solve
import Typey.Solve2
import Typey.Solve3
import Typey.Annotate
import Data.Maybe
import General.General


typeyCaseCheck :: String -> Handle -> Hite -> IO Bool
typeyCaseCheck file hndl hite =
    do funcT <- annotate file hite
       dataT <- return []
       (dataT, funcT) <- return $ liftData dataT funcT
       typeySolve file hndl hite dataT funcT


typeyHoCaseCheck :: String -> Handle -> Hite -> IO Bool
typeyHoCaseCheck file hndl hite =
    do funcT <- annotate2 file hite
       typeySolve2 file hndl hite getDatas funcT


abstractCaseCheck :: String -> Handle -> Hite -> IO Bool
abstractCaseCheck file hndl hite =
    do funcT <- annotate2 file hite
       typeySolve3 file hndl hite getDatas funcT
