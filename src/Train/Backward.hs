
module Train.Backward(backward) where

import Train.Type
import Train.Template
import System.IO
import General.General


backward :: ZHite -> Template -> Handle -> Scopes -> IO Scopes
backward zhite template hndl x = do
	hPutStrLn hndl ("Solving: " ++ output x)
	return x


