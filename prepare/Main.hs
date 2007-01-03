
module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.Cmd

import Yhc.Core


main = getArgs >>= mapM_ prepFile



prepFile :: String -> IO ()
prepFile fil = do
    fileHs <- findFileHs fil
    fileYha <- findFileYha fileHs
    compile fileHs
    overlay fileYha



findFileHs :: String -> IO FilePath
findFileHs file = do
    b <- doesFileExist file
    if b then return file else do
        let files = ["../examples" </> s </> file <.> "hs" | s <- ["Example","Nofib"]]
        bs <- mapM doesFileExist files
        case [a | (a,b) <- zip files bs, b] of
            (x:_) -> return x
            _ -> error $ "File not found, " ++ file


findFileYha :: String -> IO FilePath
findFileYha x = return $ dropFileName x </> "ycr" </> replaceExtension (takeFileName x) "yca"



compile :: String -> IO ()
compile file = do
    system $ "yhc -hide -linkcore " ++ file
    return ()


overlay :: String -> IO ()
overlay file = do
    system "yhc -hide -core ../examples/Library/Primitive.hs"
    src <- loadCore file
    over <- loadCore "../examples/Library/ycr/Primitive.ycr"
    saveCore (replaceExtension file "over.yca") $ coreReachable ["main"] $ coreOverlay src over
