
module Main where

import Yhc.Core
import System.FilePath
import System.Environment
import System.Directory
import System.IO
import Data.Proposition
import Control.Monad

import Prepare
{-
import Propagate
import Req
import Backward
import Template
-}

main = do
    xs <- getArgs
    mapM_ exec xs
    

findFile :: String -> IO FilePath
findFile file = do
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        (x:_) -> return x
        _ -> error $ "File not found, " ++ file
    where files = file : ["../examples" </> s </> "ycr" </> file <.> "letelim.yca" | s <- ["Example","Nofib"]]


exec fil = do
        file <- findFile fil
        core <- liftM prepare $ loadCore file
        
        print core
{-        

        hCore <- beginLog file "core"
        hPutStrLn hCore (show core)
        hBack <- beginLog file "back"
        hFore <- beginLog file "fore"

        template <- templateInit core hFore
        let conds = initialReqs core
        res <- mapM (f hFore hBack core template) conds
        let ress = propAnds res

        when (null conds) $
            putStrLn "No pattern match errors, trivially safe"

        let msg = show ress
            lmsg = length msg
        putStrLn $ "Final: \\forall main, " ++ msg
        when (lmsg > 200) $
            putStrLn $ "Long: " ++ show lmsg

        hClose hCore
        hClose hBack
        hClose hFore
    where
        f hFore hBack core template cond = do
            let pref = "\n\n" ++ replicate 70 '-' ++ "\n"
                msg1 = "Solving " ++ show cond
            putStrLn msg1
            hPutStrLn hFore (pref ++ msg1)
            hPutStrLn hBack (pref ++ msg1)
            
            res <- backward core template hBack [cond]
            
            let msg2 = "Result \\forall main, " ++ show res
            putStrLn msg2
            hPutStrLn hBack $ "\n" ++ msg2
            return res
            

initialReqs :: Core -> Scopes
initialReqs core = [Scope func reqs | (func,reqs,expr) <- collect core isError, not $ propIsTrue reqs]
    where
        isError (CoreApp (CorePrim "error") _) = True
        isError _ = False


beginLog :: String -> String -> IO Handle
beginLog file temp = do
    let dir = "../logs/" ++ dropExtensions (takeBaseName file)
    createDirectoryIfMissing True dir
    hndl <- openFile (dir </> temp <.> "log") WriteMode
    hSetBuffering hndl NoBuffering
    return hndl
-}
