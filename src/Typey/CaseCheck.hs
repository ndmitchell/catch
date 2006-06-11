
module Typey.CaseCheck(typeyCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.Show
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
       subTs <- return $ getSubtypesFunc dataT $ fromJust $ lookup "main" funcT
       res <- return $ typeySolve hite dataT funcT $ map ((,) "main") subTs
       error $ show res
       return False

