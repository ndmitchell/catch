
module Main where

import Yhc.Core
import System.FilePath
import System.Environment
import System.Directory
import System.IO

import Backward


main = do
    xs <- getArgs
    mapM_ exec xs
    

findFile :: String -> IO FilePath
findFile file = do
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        (x:_) -> return x
        _ -> error $ "File not found, " ++ file
    where files = file : ["../examples" </> s </> "ycr" </> file <.> "first.yca" | s <- ["Example","Nofib"]]


exec fil = do
    file <- findFile fil
    core <- loadCore file

    hCore <- beginLog file "core"
    hPutStrLn hCore (show core)
    hBack <- beginLog file "back"
    hFore <- beginLog file "fore"
    
    backward core hBack hFore

    hClose hCore
    hClose hBack
    hClose hFore




beginLog :: String -> String -> IO Handle
beginLog file temp = do
    let dir = "../logs/" ++ dropExtensions (takeBaseName file)
    createDirectoryIfMissing True dir
    openFile (dir </> temp <.> "log") WriteMode
