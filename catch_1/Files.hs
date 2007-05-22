
{-
Figure out which files a command line refers to:
# A Haskell source file
# A Text file, giving one file per line
# A directory of files
-}

module Files(findFiles) where

import Control.Monad
import Data.List
import General.General
import System.Directory
import System.Exit
import System.FilePath


findFiles :: [String] -> IO [FilePath]
findFiles = concatMapM findStartFiles


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
