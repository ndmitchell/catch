
module Front.CmdHill(cmdHill) where

import System.Environment
import Control.Monad

import Front.MakeHill
import Front.CmdLine
import Hill.All



cmdHill :: IO ()
cmdHill = do
    args <- getArgs
    cmdLine initialHill cmdsHill args
    return ()


initialHill :: CmdLineState -> FilePath -> IO ValueHill
initialHill state file = liftM ValueHill $ makeHill file
