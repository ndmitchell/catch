
module Main where

import System.FilePath
import System.Directory
import System.Environment
import System.CPUTime
import System.Cmd
import System.Exit
import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Prepare.All
import Analyse.All
import Yhc.Core
import General.CmdLine2
import General.General

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
        mapM_ (mainFile flags) =<< concatMapM findStartFiles files


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
        
        stage "Compiling"
        compile file
        core <- loadCore ycafile

        stage "Transformations"
        core <- f core 1 tasks
        
        stage "Analysing"
        (logger, close) <- if Quiet `notElem` flags then return (\a b -> return (), return ()) else do
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

        {-
        when (Regress `elem` options) $ do
            src <- readFile origfile
            let line0 = takeWhile (/= '\n') src
                start = "-- #CATCH"
                valid = start `isPrefixOf` line0
                demand = if valid then drop (length start) line0 else "_"

            when (not valid) $ putStrLn "WARNING: Regression statement not found (assuming _)"

            if result /= dropWhile isSpace demand then
                error "ERROR: Regression statement differs from answer"
             else
                putStrLn "Regression statement matches"
        -}
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
                putStrLn $ "Error: Failed in stage " ++ name
                exitFailure

            f core (i+1) tasks


tasks = [("Overlay"   , liftM success . overlay)
        ,("Firstify"  , return . firstify)
        ,("LetElim"   , return . success . letElim)
        ,("OneArg"    , return . success . oneArg)
        ,("UniqueVars", return . success . uniqueVars)
        ,("ShortCtors", return . success . shortCtors)
        ]


-- * File Location

-- find the file that the user specified on the command line:
-- # A Haskell source file
-- # A Text file, giving one file per line
-- # A directory of files
--
-- file must be in <directory>/givenname.<extension>
findStartFiles :: String -> IO [FilePath]
findStartFiles file = do
        dirs <- findStartDirs
        let exts = ["","hs","lhs","txt"]
            poss = [d </> file <.> e | d <- dirs, e <- exts]
        f poss
    where
        f [] = putStrLn ("Error: File not found, " ++ file) >> exitFailure
        f (x:xs) = do
            bFile <- doesFileExist x
            bDir  <- doesDirectoryExist x
            if bFile then (
                if takeExtension x == ".txt" then
                    readFile x >>= concatMapM findStartFiles . lines
                else
                    return [x]
             )
             else if bDir then do
                items <- getDirectoryContents x
                items <- return $ map (x </>) $ filter (\x -> takeExtension x `elem` [".hs",".lhs"]) items
                if null items then error $ "No files found within, " ++ x
                              else return items
             else f xs


findStartDirs :: IO [FilePath]
findStartDirs = do
    base <- baseDir
    let examples = base </> "examples"
    b <- doesDirectoryExist examples
    if not b then return [""] else do
        items <- getDirectoryContents examples
        items <- return $ map (examples </>) $ filter (not . isPrefixOf ".") items
        items <- filterM doesDirectoryExist items
        return ("" : examples : items)
