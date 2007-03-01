
module Prepare.Compile(compile) where

import System.Cmd
import System.Exit
import Control.Monad


compile :: FilePath -> IO ()
compile file = do
    res <- system $ "yhc -hide -linkcore " ++ file
    when (res /= ExitSuccess) $ error "Failed to compile"

