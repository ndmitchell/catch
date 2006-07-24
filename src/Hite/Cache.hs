
module Hite.Cache(readCacheHite, writeCacheHite) where

import Hite.Type
import Directory
import Monad
import General.General


readCacheHite :: FilePath -> IO Hite
readCacheHite file = do src <- readFile file
                        return $ readNote ("Failed while reading cached hite: " ++ file) src


writeCacheHite :: Hite -> FilePath -> IO ()
writeCacheHite hite file = do b <- doesFileExist file
                              when b $ removeFile file
                              writeFile filet (show hite)
                              renameFile filet file
    where
        filet = file ++ ".tmp"

