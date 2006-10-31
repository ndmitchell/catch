
module Front.CmdHill(cmdHill) where

import System.Directory

import Front.MakeHill
import Front.CmdLine
import Hill.All
import Hill.Check



cmdHill :: [String] -> IO ()
cmdHill args = do
    cmdLine initialHill (map insertCheck cmdsHill) args
    return ()


insertCheck :: Action Value -> Action Value
insertCheck (Action name exec) = Action name exec2
    where
        exec2 :: CmdLineState -> String -> Value -> IO Value
        exec2 state arg (ValueHill x) | "check" `elem` cmdLineOptions state = exec state arg (ValueHill $ hillCheck x)
        exec2 state arg x = exec state arg x



initialHill :: CmdLineState -> FilePath -> IO Value
initialHill _ x = do
    let file1 = "Example/" ++ x ++ ".hs"
        file2 = "Nofib/" ++ x ++ ".hs"
        files = [x, file1, file2]
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        [] -> error $ "File not found, " ++ x
        (x:_) -> return $ ValueFile x
