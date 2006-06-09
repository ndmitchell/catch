
module Typey.CaseCheck(typeyCaseCheck) where

import IO
import Hite
import Typey.Type
import Typey.Annotate
import General.General


typeyCaseCheck :: String -> Handle -> Hite -> IO Bool
typeyCaseCheck file hndl hite = annotate hite

