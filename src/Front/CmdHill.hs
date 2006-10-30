
module Front.CmdHill(cmdHill) where

import System.Directory

import Front.MakeHill
import Front.CmdLine
import Hill.All



cmdHill :: [String] -> IO ()
cmdHill args = do
    cmdLine initialHill cmdsHill args
    return ()


initialHill :: CmdLineState -> FilePath -> IO Value
initialHill _ x = do
    let file1 = "Example/" ++ x ++ ".hs"
        file2 = "Nofib/" ++ x ++ ".hs"
        files = [x, file1, file2]
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        [] -> error $ "File not found, " ++ x
        (x:_) -> return $ ValueFile x
