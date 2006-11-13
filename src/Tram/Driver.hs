
module Tram.Driver(tramDriver) where

import System.IO
import Data.Proposition
import Control.Monad

import Tram.Template
import Tram.Type
import Tram.Backward
import Tram.Propagate
import Hill.All
import General.General
import Front.CmdLine


tramDriver :: CmdLineState -> Hill -> IO Bool
tramDriver cmdState hill = do
        hFlush hndl
        hndlTemplate <- openFile (logFile "template") WriteMode
        hndlBackward <- openFile (logFile "backward") WriteMode
        
        hSetBuffering hndl LineBuffering
        hSetBuffering hndlTemplate LineBuffering
        hSetBuffering hndlBackward LineBuffering
        
        template <- templateInit hill hndlTemplate
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
        hndl = cmdLineHandle cmdState
        conds = initialReqs hill
        logFile x = cmdLineOutput cmdState x
        
        outBoth msg = hPutStrLn hndl msg >> putStrLn msg
        
        f hndlTemplate hndlBackward template cond = do
            let pref = "\n\n" ++ replicate 70 '-' ++ "\n"
                msg1 = "Solving " ++ show cond
            outBoth msg1
            hPutStrLn hndlTemplate (pref ++ msg1)
            hPutStrLn hndlBackward (pref ++ msg1)
            
            res <- backward hill template hndlBackward [cond]
            
            let msg2 = "Result \\forall main, " ++ show res
            outBoth msg2
            hPutStrLn hndlBackward $ "\n" ++ msg2
            return res
            



initialReqs :: Hill -> [Scope]
initialReqs hill = [Scope func (reqsNot reqs) | (func,reqs,expr) <- collect hill isError]
