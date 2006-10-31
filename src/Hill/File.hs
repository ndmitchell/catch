
module Hill.File(cmdsFile) where

import Hill.Type
import Hill.Cache
import Yhc.Core
import Front.CmdLine
import Control.Monad
import System.FilePath
import System


cmdsFile =
    [Action "core-make" coreMake
    ,Action "core-load" coreLoad
    ,Action "overlay-make" overlayMake
     -- ,Action "core-save" coreSave
    ,Action "hill-load" hillLoad
    ,Action "hill-save" hillSave
    ]



coreMake :: CmdLineState -> String -> Value -> IO Value
coreMake state _ (ValueFile x) = system ("yhc -hide -linkcore " ++ x) >> return (ValueFile x)

overlayMake :: CmdLineState -> String -> Value -> IO Value
overlayMake state _ val = system "yhc -hide -core Library/Primitive.hs" >> return val


coreLoad :: CmdLineState -> String -> Value -> IO Value
coreLoad state _ (ValueFile x) = liftM ValueCore $ loadCore x2
    where x2 = dropFileName x </> "ycr" </> getBaseName x <.> "yca"



hillLoad :: CmdLineState -> String -> Value -> IO Value
hillLoad state _ _ = liftM ValueHill $ readCacheHill $ cmdLineOutput state "hill"

hillSave :: CmdLineState -> String -> Value -> IO Value
hillSave state _ value = writeCacheHill (valueHill value) (cmdLineOutput state "hill") >> return value
