
module Hill.File(cmdsFile) where

import Hill.Type
import Hill.Cache
import Front.CmdLine
import Control.Monad
import System.FilePath
import System


cmdsFile =
    [Action "core-make" coreMake
     -- ,Action "core-load" coreLoad
     -- ,Action "core-save" coreSave
    ,Action "hill-load" hillLoad
    ,Action "hill-save" hillSave
    ]



coreMake :: CmdLineState -> String -> Value -> IO Value
coreMake state _ (ValueFile x) = system ("yhc -hide -linkcore " ++ x) >> return (ValueFile x)


hillLoad :: CmdLineState -> String -> Value -> IO Value
hillLoad state _ _ = liftM ValueHill $ readCacheHill $ cmdLineOutput state "hill"

hillSave :: CmdLineState -> String -> Value -> IO Value
hillSave state _ value = writeCacheHill (valueHill value) (cmdLineOutput state "hill") >> return value
