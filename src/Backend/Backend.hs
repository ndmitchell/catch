
module Backend.Backend(backend) where

import System.IO
import Hite


backend :: String -> Handle -> Hite -> IO Bool
backend file hndl hite = do
        writeFile ("Logs/" ++ file ++ ".c") (toC hite)
        return True


toC :: Hite -> String
toC hite = unlines ["todo"]
