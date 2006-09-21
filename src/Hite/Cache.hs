
module Hite.Cache(readCacheHite, writeCacheHite) where

import Hite.Type
import Directory
import Monad
import General.General

import Hite.Binary
import General.Binary


useBinary = True


readCacheHite :: FilePath -> IO Hite
readCacheHite file = readBinary file
{-
                   | otherwise = do
    src <- readFile file
    return $ readNote ("Failed while reading cached hite: " ++ file) src
-}

writeCacheHite :: Hite -> FilePath -> IO ()
writeCacheHite hite file = do b <- doesFileExist file
                              when b $ removeFile file
                              --if useBinary then writeBinary filet hite else writeFile filet (show hite)
                              writeBinary filet hite
                              renameFile filet file
    where
        filet = file ++ ".tmp"

