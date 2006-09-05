
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
		template <- templateInit zhite hndlTemplate
		res <- mapM (backward zhite template hndlBackward . (:[]) ) conds
		when (null conds) $
			putStrLn "No pattern match errors, trivially safe"
		
		putStrLn $ "Final: " ++ (unlines $ map output $ concat res)
		
		hFlush hndl
		hClose hndlTemplate
		hClose hndlBackward
		return False -- $ all propIsTrue res
	where
		conds = initialReqs zhite
		zhite = convertHite hite
		logFile x = "Logs/" ++ file ++ "." ++ x ++ ".log"



initialReqs :: ZHite -> [Scope]
initialReqs (ZHite _ funcs) = concatMap f funcs
	where
		f (ZFunc name _ alts) = [Scope name (reqsNot cond) | (cond,Left _) <- alts]
