
module Hite.Cache(readCacheHite, writeCacheHite) where

import Hite.Type
import Directory
import Monad


readCacheHite :: FilePath -> IO Hite
readCacheHite file = do src <- readFile file
                        return $ safeRead "Failed while reading cached hite" src


safeRead msg s = case reads s of
                    [(x,"")] -> x
                    _ -> error msg
                    


writeCacheHite :: Hite -> FilePath -> IO ()
writeCacheHite hite file = do b <- doesFileExist file
                              when b $ removeFile file
                              writeFile filet (show hite)
                              renameFile filet file
    where
        filet = file ++ ".tmp"

