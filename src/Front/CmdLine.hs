
module Front.CmdLine(cmdLine, CmdLineState(..), Action(..)) where

import Control.Exception
import System.IO
import Data.List
import Data.Char
import Control.Monad



data CmdLineState = CmdLineState
    {cmdLineHandle :: Handle -- the handle of the open file
    ,cmdLineLogging :: String -> FilePath -- generate a log file near this file
    ,cmdLineOptions :: [String] -- options passed
    }
    

data Action val = Action
    {actionName :: String
    ,actionExec :: CmdLineState -> String -> val -> IO val}



cmdLine :: Show val => (CmdLineState -> FilePath -> IO val) -> [Action val] -> [String] -> IO [val]
cmdLine initial actions cmds =
    do
        () <- assert (length actions == length (nub $ map actionName actions)) $ return ()
        aliases <- loadAliases
        when (null files) $ putStrLn "No files, nothing to do"
        mapM (f aliases) files
    where
        (acts,other) = partition ("-" `isPrefixOf`) cmds
        (opts,files) = partition ("@" `isPrefixOf`) other

        f aliases file = do
            hndl <- openFile (logFile file "") WriteMode
            hPutStrLn hndl $ "-- Catch log file, " ++ file
            let state = CmdLineState hndl (\x -> logFile file ('.':x)) (map tail opts)
            
            x <- initial state file
            x <- g aliases state x (map tail acts)
            
            hPutStrLn hndl "-- Result:"
            hPutStrLn hndl (show x)
            hClose hndl
            print x
            return x


        g aliases state val [] = return val
        g aliases state val (a:cts) =
            let (a1,a2) = break (== '=') a in
            case lookup a aliases of
                Just q -> g aliases state val (q++cts)
                Nothing -> case [x | x <- actions, actionName x == a1] of
                    [] -> error $ "Command not found, " ++ a
                    (x:_) -> do
                        let hndl = cmdLineHandle state
                        putStrLn $ "-- Executing " ++ a
                        hPutStrLn hndl $ "-- Executing " ++ a
                        val <- (actionExec x) state (drop 1 a2) val
                        hPutStrLn hndl $ show val
                        hPutStrLn hndl $ replicate 70 '-'
                        g aliases state val cts

        
        logFile :: String -> String -> FilePath
        logFile source tag = "Logs/" ++ source ++ tag ++ ".log"



loadAliases :: IO [(String,[String])]
loadAliases =
    do
        src <- readFile "catch.txt"
        return $ tail $ f ("",[]) $ filter (not.null) $ dropWhile null $ lines src
    where
        f acc [] = [acc]
    
        f acc@(name,cmds) ((x1:xs):ss)
            | isSpace x1 =
                f (name, cmds ++ ['-':dropWhile isSpace xs]) ss
        
            | otherwise =
                acc : f (newComp (x1:xs)) ss
        
        newComp xs | null b = (xs,[])
                   | otherwise = (a,[])
            where (a,b) = break isSpace xs
