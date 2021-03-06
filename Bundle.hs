-- Make a complete bundle of all the source modules to build Catch and create a package

import System.Directory
import System.Environment
import System.Cmd
import System.IO
import Control.Monad
import System.FilePath
import Data.List
import Data.Char


main = do
    b <- doesDirectoryExist "release"
    when b $ removeDirectoryRecursive "release"
    createDirectory "release"
    createDirectory "release/catch"

    yhc <- getEnv "YHC_BASE_PATH"
    copyHaskell "catch_1" "release/catch/src"
    copyHaskell "proposition" "release/catch/src"
    copyHaskell "examples" "release/catch/examples"
    copyHaskell (yhc ++ "/../src/libraries/core") "release/catch/src"
    copyHaskell (yhc ++ "/../depends/uniplate") "release/catch/src"
    copyFileBinary "catch.htm" "release/catch/catch.htm"
    renameFile "release/catch/src/Setup.hs" "release/catch/Setup.hs"
    createCabal

    system "cd release && tar -cf catch.tar catch"


badDirs = ["Examples","Benchmark","hw2007","Unused","Dead"]
badFiles = ["Test","Paths_catch"]


copyHaskell :: FilePath -> FilePath -> IO ()
copyHaskell from to = do
    let check s = null (splitDirectories s `intersect` badDirs) &&
                  takeExtension s `elem` [".hs",".lhs"] &&
                  takeBaseName s `notElem` badFiles &&
                  all isAlphaNum (concat $ splitDirectories $ dropExtension s)
    files <- liftM (filter check) $ listFiles from

    flip mapM_ files $ \s -> do
        createDirectoryIfMissing True (takeDirectory $ to </> s)
        copyFileBinary (from </> s) (to </> s)


listFiles :: FilePath -> IO [FilePath]
listFiles root = do
    s <- getDirectoryContents root
    liftM concat $ flip mapM s $ \s -> do
        dir <- doesDirectoryExist (root </> s)
        if dir then
            if head s `elem` "._" then
                return []
            else
                liftM (map (s </>)) $ listFiles (root </> s)
         else
            return [s]


createCabal :: IO ()
createCabal = do
    src <- readFile "bundle.txt"
    
    let indent = (++) "    "
        f = indent . concat . intersperse "." . splitDirectories . dropExtension
        g x = if x == '\\' then '/' else x
    mods <- liftM (map f) $ listFiles "release/catch/src"
    fils <- liftM (map (indent . map g . (</>) "examples")) $ listFiles "release/catch/examples"
    
    let ask "EXAMPLES" = fils
        ask "MODULES" = mods

        f ('$':xs) = ask xs
        f xs = [xs]
    
    h <- openBinaryFile "release/catch/catch.cabal" WriteMode
    hPutStr h $ unlines $ concatMap f $ lines src
    hClose h


copyFileBinary from to = do
    hIn  <- openBinaryFile from ReadMode
    s <- hGetContents hIn
    hOut <- openBinaryFile to WriteMode
    hPutStr hOut s
    hClose hIn
    hClose hOut
