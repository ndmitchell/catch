
module Train.Driver(trainDriver) where

import System.IO
import Data.Proposition
import Control.Monad

import Train.Template
import Train.Type
import Train.Convert
import Train.Backward
import Hite
import General.General


trainDriver :: String -> Handle -> Hite -> IO Bool
trainDriver file hndl hite = do
        hFlush hndl
        hndlTemplate <- openFile (logFile "template") WriteMode
        hndlBackward <- openFile (logFile "backward") WriteMode
        
        hSetBuffering hndl LineBuffering
        hSetBuffering hndlTemplate LineBuffering
        hSetBuffering hndlBackward LineBuffering
        
        template <- templateInit zhite hndlTemplate
        res <- mapM (f hndlTemplate hndlBackward template) conds
        let ress = propAnds res

        when (null conds) $
            putStrLn "No pattern match errors, trivially safe"

        outBoth $ "Final: \\forall main, " ++ show ress

        hFlush hndl
        hClose hndlTemplate
        hClose hndlBackward
        return $ propIsTrue ress
    where
        conds = initialReqs zhite
        zhite = convertHite hite
        logFile x = "Logs/" ++ file ++ "." ++ x ++ ".log"
        
        outBoth msg = hPutStrLn hndl msg >> putStrLn msg
        
        f hndlTemplate hndlBackward template cond = do
            let pref = "\n\n" ++ replicate 70 '-' ++ "\n"
                msg1 = "Solving " ++ show cond
            outBoth msg1
            hPutStrLn hndlTemplate (pref ++ msg1)
            hPutStrLn hndlBackward (pref ++ msg1)
            
            res <- backward zhite template hndlBackward [cond]
            
            let msg2 = "Result \\forall main, " ++ show res
            outBoth msg2
            hPutStrLn hndlBackward $ "\n" ++ msg2
            return res
            



initialReqs :: ZHite -> [Scope]
initialReqs (ZHite _ funcs) = concatMap f funcs
	where
		f (ZFunc name _ alts) = [Scope name (reqsNot cond) | (cond,Left _) <- alts]
