
module Main where

import System.Environment
import System.FilePath
import System.Directory
import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Prepare.All
import Analyse.All
import Yhc.Core
import General.CmdLine
import General.General



main :: IO ()
main = do
    xs <- getArgs
    let (files,stages,options) = parseCommandLine xs
    if null files
        then helpMessage
        else do
            files <- concatMapM findStartFiles files
            mapM_ (execFile stages options) files


execFile :: [Stage] -> [Option] -> String -> IO ()
execFile stages options origfile = do
        putStrLn $ "Executing: " ++ origfile
        let origfile_yha = dropFileName origfile </> "ycr" </> replaceExtension (takeFileName origfile) "yca"
        
        -- compile
        ycafile <-
            if takeExtension origfile == ".yca" then return origfile
            else if Compile `elem` stages then do
                when (Quiet `notElem` options) $ putStrLn "Compiling"
                compile origfile
                return origfile_yha
            else do
                b <- doesFileExist origfile_yha
                if b then return origfile_yha
                     else error $ "Compilation not specified, but file not compiled: " ++ origfile

        -- the middle bit
        res <- execMiddle stages options ycafile
        
        -- analysis
        when (Analyse `elem` stages) $ do
            when (Quiet `notElem` options) $ putStrLn "Analysing"
            core <- loadStage ycafile res (pred Analyse)
            (logger, close) <- if NoLog `elem` options then return (\a b -> return (), return ()) else do
                let openLog x = do h <- openFile (replaceExtension ycafile (x <.> "log")) WriteMode
                                   hSetBuffering h NoBuffering
                                   return h
                hPre  <- openLog "pre"
                hProp <- openLog "prop"
                
                let logger b msg = do when (Screen `elem` options) $
                                          putStrLn $ (if b then "" else "    ") ++ msg
                                      hPutStrLn (if b then hPre else hProp) msg
                
                return (logger, hClose hPre >> hClose hProp)

            result <- analyse logger core
            putStrLn $ "Answer: " ++ result
            close
            when (Regress `elem` options) $ do
                src <- readFile origfile
                let line0 = takeWhile (/= '\n') src
                    start = "-- #CATCH"
                if not $ start `isPrefixOf` line0 then
                    error "ERROR: Regression statement not found"
                 else if result /= dropWhile isSpace (drop (length start) line0) then
                    error "ERROR: Regression statement differs from answer"
                 else
                    putStrLn "Regression statement matches"



-- load the result of doing a particular stage
loadStage :: FilePath -> Maybe (Stage,Core) -> Stage -> IO Core
loadStage file (Just (s,c)) stage | s == stage = return c
loadStage file _ stage = loadCore $ whereStage file stage <.> "yca"


whereStage :: FilePath -> Stage -> FilePath
whereStage file stage = replaceExtension file $ map toLower (if stage == Compile then "" else show stage)


execMiddle :: [Stage] -> [Option] -> FilePath -> IO (Maybe (Stage,Core))
execMiddle stages options file = fs [succ Compile .. pred Analyse] Nothing
    where
        fs [] prev = return prev
        fs (s:ss) prev = f s prev >>= fs ss
    
        f stage prev | stage `notElem` stages = return Nothing
                     | otherwise = do
            core <- loadStage file prev (pred stage)
            when (Quiet `notElem` options) $ putStrLn $ "Task: " ++ show stage
            (success,core) <- getTask stage core
            
            let out = whereStage file stage
            when (Text `elem` options) $ writeFile (out <.> "txt" ) (show core)
            when (Html `elem` options) $ writeFile (out <.> "html") (coreHtml core)
            when (Yca  `elem` options) $ saveCore  (out <.> "yca" ) core
            if success then return $ Just (stage,core) else error $ "Failed in stage " ++ show stage



tasks = [(Overlay   , liftM success . overlay)
        ,(Firstify  , return . firstify)
        ,(LetElim   , return . success . letElim)
        ,(OneArg    , return . success . oneArg)
        ,(UniqueVars, return . success . uniqueVars)
        ,(ShortCtors, return . success . shortCtors)
        ]

getTask :: Stage -> (Core -> IO (Result Core))
getTask x = fromJust $ lookup x tasks




-- find the file that the user specified on the command line
-- this file will be either a .hs file (to compile) or a .yca (already compiled)
--
-- file must be in <directory>/givenname.<extension>
findStartFiles :: String -> IO [FilePath]
findStartFiles file = do
        dirs <- findStartDirs
        let exts = ["","hs","lhs","yca"]
            poss = [d </> file <.> e | d <- dirs, e <- exts]
        f poss
    where
        f [] = error $ "File not found, " ++ file
        f (x:xs) = do
            bFile <- doesFileExist x
            bDir  <- doesDirectoryExist x
            if bFile then return [x]
             else if bDir then do
                items <- getDirectoryContents x
                items <- return $ map (x </>) $ filter (\x -> takeExtension x `elem` [".hs",".lhs"]) items
                if null items then error $ "No files found within, " ++ x
                              else return items
             else f xs


findStartDirs :: IO [FilePath]
findStartDirs = do
    let examples = ".." </> "examples"
    b <- doesDirectoryExist examples
    if not b then return [""] else do
        items <- getDirectoryContents examples
        items <- return $ map (examples </>) $ filter (not . isPrefixOf ".") items
        items <- filterM doesDirectoryExist items
        return ("" : examples : items)
