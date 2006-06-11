
module Typey.CaseCheck(typeyCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.Show
import Typey.Annotate
import General.General


typeyCaseCheck :: String -> Handle -> Hite -> IO Bool
typeyCaseCheck file hndl hite = do res <- annotate file hite
                                   error $ show res
                                   return False

