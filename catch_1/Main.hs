
module Main where

import Analyse.All
import Control.Monad
import Data.List
import Files
import Haskell
import General.CmdLine
import General.General
import Prepare.All
import System.CPUTime
import System.Cmd
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Yhc.Core

{-
The flags which need special processing:

Profile - restart with +RTS -p straight away

Memory - restart upon each file with a memory computation
-}


-- * Main Start Routine

main :: IO ()
main = do
    (flags,files) <- parseCmdLine
    if DProfile `elem` flags then
        execProfile flags files
     else
        mapM_ (mainFile flags) =<< findFiles files


mainFile :: [Flag] -> FilePath -> IO ()
mainFile flags file = do
    if DMemory `elem` flags then
        execMemory flags file
     else if DTime `elem` flags then
        execTime flags file
     else
        execFile flags file


-- * Rerun Commands

rerun suffix cmdline = do
    progName <- getProgName
    system $ dropExtension progName ++ suffix ++ " " ++ cmdline
    return ()


execProfile flags files = do
    rerun "_prof " $ unparseCmdLine (delete DProfile flags) files ++ " +RTS -p"

execMemory flags file = do
    progName <- getProgName
    let stat = progName <.> "stat"
    b <- doesFileExist stat
    when b $ removeFile stat
    rerun "" $ unparseCmdLine (delete DMemory flags) [file] ++ " +RTS -t"
    src <- readFileStrict stat
    let res = takeWhile (/= ' ') $ drop 1 $ dropWhile (/= '/') $ lines src !! 1
    putStrLn $ "Memory used: " ++ (twoDp $ fromInteger (read res) / 1024) ++ " Kb"

execTime flags file = do
    startTime <- getCPUTime
    execFile (delete DTime flags) file
    endTime <- getCPUTime
    let diff = fromInteger (endTime - startTime) * 1e-12
    putStrLn $ "Time taken: " ++ twoDp diff ++ " seconds"


-- * Standard Execution

execFile :: [Flag] -> FilePath -> IO ()
execFile flags file = do
        putStrLn $ "Executing: " ++ file
        Pragmas exports partial regress <- liftM parseHaskell $ readFile file
        
        stage "Compiling"
        compile file $ unwords [s | Yhc s <- flags]
        core <- liftM (createMain $ exports \\ partial) $ loadCore ycafile

        stage "Transformations"
        core <- f core 1 tasks
        
        stage "Analysing"
        (logger, close) <- if DLog `notElem` flags then return (\a b -> return (), return ()) else do
            let openLog x = do h <- openFile (replaceExtension ycafile (x <.> "log")) WriteMode
                               hSetBuffering h NoBuffering
                               return h
            hPre  <- openLog "pre"
            hProp <- openLog "prop"

            let logger b msg = hPutStrLn (if b then hPre else hProp) msg

            return (logger, hClose hPre >> hClose hProp)

        result <- analyse logger flags core
        putStrLn $ "Answer: " ++ result
        close

        when (regress /= "" && regress /= result) $ do
            putStrLn $ "ERROR: Regression statement differs from answer, expected " ++ regress
            exitFailure
    where
        ycafile = dropFileName file </> "ycr" </> replaceExtension (takeFileName file) "yca"
        stage msg = when (Quiet `notElem` flags) $ putStrLn msg
        
        f core i [] = return core
        f core i ((name,exec):tasks) = do
            stage $ "Task: " ++ name
            (success,core) <- exec core

            when (DCore `elem` flags) $ do
                let out = replaceExtension ycafile $ "." ++ show i ++ "." ++ name
                writeFile (out <.> "txt" ) (show core)
                writeFile (out <.> "html") (coreHtml core)
                saveCore  (out <.> "yca" ) core

            when (not success) $ do
                putStrLn $ "Warning: Failed in stage " ++ name

            f core (i+1) tasks


tasks = [("Overlay"   , liftM success . overlay)
        ,("Firstify"  , return . firstify)
        ,("LetElim"   , return . success . letElim)
        ,("OneArg"    , return . success . oneArg)
        ,("UniqueVars", return . success . uniqueVars)
        ,("ShortCtors", return . success . shortCtors)
        ]
