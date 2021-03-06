
module General.Output where

import Control.Monad.Reader
import Data.IORef
import IO
import Options


type OutputMonad a = ReaderT Vars IO a


data Vars = Vars {
    depth :: IORef Int,
    handle :: Handle
}



runOutput :: Handle -> OutputMonad a -> IO a
runOutput hndl out = do depth <- newIORef 0
                        hSetBuffering hndl NoBuffering
                        runReaderT out (Vars depth hndl)



getHandle :: OutputMonad Handle
getHandle = do x <- ask
               return (handle x)



putBoth :: String -> OutputMonad ()
putBoth msg = do when (not printEverything) $ liftIO $ putStrLn msg
                 putLog msg

putLog :: String -> OutputMonad ()
putLog msg = do hndl <- getHandle
                ind <- getIndent
                let msg2 = replicate (ind*2) ' ' ++ msg
                liftIO $ hPutStrLn hndl msg2
                when printEverything $ liftIO $ putStrLn msg2
                

incIndent :: OutputMonad ()
incIndent = addIndent (+1)

decIndent :: OutputMonad ()
decIndent = addIndent (\n -> n-1)


addIndent :: (Int -> Int) -> OutputMonad ()
addIndent f = do x <- ask
                 liftIO $ modifyIORef (depth x) f

getIndent :: OutputMonad Int
getIndent = do x <- ask
               liftIO $ readIORef (depth x)


outputIO a = liftIO a


{-
main = runOutput stdout sample


sample = do putBoth "test"
            incIndent
            putLog "neil"
            decIndent
            putLog "test2"
-}
