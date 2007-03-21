
module Analyse.All(analyse) where

import Yhc.Core
import Analyse.Info
import Analyse.Property
import Analyse.Precond


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
analyse :: (Bool -> String -> IO ()) -> Core -> IO String
analyse logger core = do
    initInfo core
    initProperty (logger False)
    res <- precond (logger True) (const True) (map coreFuncName $ coreFuncs core)
    termInfo
    termProperty
    return $ show res
