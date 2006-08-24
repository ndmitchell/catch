
module Train.Driver(trainDriver) where

import System.IO
import Data.Predicate

import Train.Template
import Train.Type
import Train.Convert
import Train.Backward
import Hite


trainDriver :: String -> Handle -> Hite -> IO Bool
trainDriver file hndl hite = do
		hndlTemplate <- openFile (logFile "template") WriteMode
		hndlBackward <- openFile (logFile "backward") WriteMode
		template <- templateInit zhite hndlTemplate
		res <- mapM (backward zhite template hndlBackward) conds
		return $ all isTrue res
	where
		conds = initialReqs zhite
		zhite = convertHite hite
		logFile x = "Logs/" ++ file ++ "." ++ x ++ ".log"



initialReqs :: ZHite -> [Scopes]
initialReqs (ZHite _ funcs) = concatMap f funcs
	where
		f (ZFunc name _ alts) = [predLit $ Scope name (predNot cond) | (cond,Left _) <- alts]
