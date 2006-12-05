
module Main where

import Yhc.Core
import System.Environment
import System.IO
import Control.Monad

import Prepare
-- import Backward


main = do
    xs <- getArgs
    when (null xs) $ error "Please give list of files to use"
    mapM_ exec xs
    

exec file = do
    ov <- loadCore "../examples/Library/ycr/Primitive.ycr"
    cr <- loadCore $ "../examples/Example/ycr/" ++ file ++ ".yca"
    let core = prepare $ coreReachable ["main"] $ coreOverlay cr ov
    
    hCore <- beginLog file "core"
    hPutStrLn hCore (show core)
    hBack <- beginLog file "back"
    hFore <- beginLog file "fore"
    
    -- backward core hBack hFore

    hClose hCore
    hClose hBack
    hClose hFore




beginLog :: String -> String -> IO Handle
beginLog file temp = openFile ("../logs/" ++ file ++ "/" ++ temp ++ ".log") WriteMode
