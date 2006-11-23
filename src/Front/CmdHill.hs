
module Front.CmdHill(cmdHill) where

import System.Directory
import Control.Monad

import Front.MakeHill
import Front.CmdLine
import Tram.Driver
import Hill.All
import Hill.Check


allCmds = cmdsHill ++ cmdsAnalysis

cmdHill :: [String] -> IO ()
cmdHill args = do
    cmdLine initialHill (map insertCheck allCmds) args
    return ()


insertCheck :: Action Value -> Action Value
insertCheck (Action name exec) = Action name exec2
    where
        exec2 :: CmdLineState -> String -> Value -> IO Value
        exec2 state arg (ValueHill x) | "check" `elem` cmdLineOptions state = exec state arg (ValueHill $ hillCheck x)
        exec2 state arg x = exec state arg x



initialHill :: CmdLineState -> FilePath -> IO Value
initialHill _ x = do
    let file1 = "../examples/Example/" ++ x ++ ".hs"
        file2 = "../examples/Nofib/" ++ x ++ ".hs"
        files = [x, file1, file2]
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        [] -> error $ "File not found, " ++ x ++ "\nLooked at " ++ show files
        (x:_) -> return $ ValueFile x


cmdsAnalysis = [Action "tram-patterns" f]
    where
        f state arg (ValueHill x) = liftM ValueBool $ tramDriver state x
