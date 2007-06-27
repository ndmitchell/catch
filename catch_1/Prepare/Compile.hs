
module Prepare.Compile(compile) where

import System.Cmd
import System.Exit
import Control.Monad


compile :: FilePath -> String -> IO ()
compile file flags = do
    res <- system $ "yhc --hide --linkcore \"" ++ file ++ "\" " ++ flags
    when (res /= ExitSuccess) $ error $ "Failed to compile, " ++ file
