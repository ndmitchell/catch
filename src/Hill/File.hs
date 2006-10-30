
module Hill.File(cmdsFile) where

import Hill.Type
import Hill.Cache
import Front.CmdLine
import Control.Monad


cmdsFile =
    [-- Action "core-make" coreMake
     -- ,Action "core-load" coreLoad
     -- ,Action "core-save" coreSave
     Action "hill-load" hillLoad
    ,Action "hill-save" hillSave
    ]




hillLoad :: CmdLineState -> String -> Value -> IO Value
hillLoad state _ _ = liftM ValueHill $ readCacheHill $ cmdLineOutput state "hill"

hillSave :: CmdLineState -> String -> Value -> IO Value
hillSave state _ value = writeCacheHill (valueHill value) (cmdLineOutput state "hill") >> return value
