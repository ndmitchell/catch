
module Analyse.All(analyse) where

import Yhc.Core
import Analyse.Info
import Analyse.Property
import Analyse.Precond


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
analyse :: (Bool -> String -> IO ()) -> Core -> IO ()
analyse logger core = do
    initInfo core
    initProperty (logger False)
    res <- precond (logger True) (map coreFuncName $ coreFuncs core)
    putStrLn $ "Answer: " ++ show res
    termInfo
    termProperty

