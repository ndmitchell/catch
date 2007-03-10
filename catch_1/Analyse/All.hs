
module Analyse.All(analyse) where

import Yhc.Core


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
analyse :: (Bool -> String -> IO ()) -> Core -> IO ()
analyse logger core = error "analyse, to do"
