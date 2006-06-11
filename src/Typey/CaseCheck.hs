
module Typey.CaseCheck(typeyCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.Show
import Typey.LiftData
import Typey.Solve
import Typey.Annotate
import General.General


typeyCaseCheck :: String -> Handle -> Hite -> IO Bool
typeyCaseCheck file hndl hite = do funcT <- annotate file hite
                                   dataT <- return []
                                   (dataT, funcT) <- return $ liftData dataT funcT
                                   error $ show (dataT, funcT)
                                   return False

