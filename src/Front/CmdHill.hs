
module Front.CmdHill(cmdHill) where

import Control.Monad

import Front.MakeHill
import Front.CmdLine
import Hill.All



cmdHill :: [String] -> IO ()
cmdHill args = do
    cmdLine initialHill cmdsHill args
    return ()


initialHill :: CmdLineState -> FilePath -> IO Value
initialHill state file = return $ ValueFile file
