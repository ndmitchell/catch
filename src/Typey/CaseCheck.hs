
module Typey.CaseCheck(typeyCaseCheck, typeyHoCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.LiftData
import Typey.Solve
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
--       dataT <- return []
--       (dataT, funcT) <- return $ liftData2 dataT funcT
       error $ showLines funcT
       