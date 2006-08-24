
module Train.Backward(backward) where

import Train.Type
import Train.Template
import System.IO


backward :: ZHite -> Template -> Handle -> Scopes -> IO Scopes
backward zhite template hndl x = do
	hPutStrLn hndl ("Solving: " ++ show x)
	return x


