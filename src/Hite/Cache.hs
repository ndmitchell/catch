
module Hite.Cache(readCacheHite, writeCacheHite) where

import Hite.Type
import Directory
import Monad


readCacheHite :: FilePath -> IO Hite
readCacheHite file = do src <- readFile file
                        return $ read src


writeCacheHite :: Hite -> FilePath -> IO ()
writeCacheHite hite file = do b <- doesFileExist file
                              when b $ removeFile file
                              writeFile filet (show hite)
                              renameFile filet file
    where
        filet = file ++ ".tmp"

