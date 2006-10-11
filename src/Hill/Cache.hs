
module Hill.Cache(readCacheHill, writeCacheHill) where

import Hill.Type
import System.Directory
import Control.Monad
import General.General

import Hill.Binary
import General.Binary


readCacheHill :: FilePath -> IO Hill
readCacheHill file = readBinary file


writeCacheHill :: Hill -> FilePath -> IO ()
writeCacheHill hill file = do b <- doesFileExist file
                              when b $ removeFile file
                              writeBinary filet hill
                              renameFile filet file
    where
        filet = file ++ ".tmp"

