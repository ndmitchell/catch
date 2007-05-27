-- Make a complete bundle of all the source modules to build Catch and create a package

import System.Directory
import System.Environment
import Control.Monad
import System.FilePath
import Data.List
import Data.Char


main = do
    b <- doesDirectoryExist "release"
    when b $ removeDirectoryRecursive "release"
    createDirectory "release"

    yhc <- getEnv "YHC_BASE_PATH"
    copyHaskell "catch_1" "release/src"
    copyHaskell "proposition" "release/src"
    copyHaskell "examples" "release/examples"
    copyHaskell (yhc ++ "/../src/libraries/general") "release/src"
    copyHaskell (yhc ++ "/../src/libraries/core") "release/src"
    copyHaskell (yhc ++ "/../depends/play") "release/src"
    copyFile "catch.htm" "release/catch.htm"
    renameFile "release/src/Setup.hs" "release/Setup.hs"
    
    createCabal


badDirs = ["Examples","Benchmark","hw2007","Unused","Dead"]
badFiles = ["Test"]


copyHaskell :: FilePath -> FilePath -> IO ()
copyHaskell from to = do
    let check s = null (splitDirectories s `intersect` badDirs) &&
                  takeExtension s `elem` [".hs",".lhs"] &&
                  takeBaseName s `notElem` badFiles &&
                  all isAlphaNum (concat $ splitDirectories $ dropExtension s)
    files <- liftM (filter check) $ listFiles from

    flip mapM_ files $ \s -> do
        createDirectoryIfMissing True (takeDirectory $ to </> s)
        copyFile (from </> s) (to </> s)


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
    let f = (++) "    " . concat . intersperse "." . splitDirectories . dropExtension
    mods <- liftM (map f) $ listFiles "release/src"
    writeFile "release/catch.cabal" $ src ++ unlines mods
