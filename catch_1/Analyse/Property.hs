
module Analyse.Property(
    initProperty, termProperty, property
    ) where

import Foreign
import Yhc.Core
import Data.IORef
import Data.Maybe
import Data.Proposition
import Control.Monad
import qualified Data.Map as Map

import Analyse.Req
import Analyse.Info
import Analyse.Back
import Analyse.Fix


type Key = (CoreFuncName, Constraint)


props :: IORef (String -> IO (), Map.Map Key Constraint)
props = unsafePerformIO $ newIORef undefined


initProperty :: (String -> IO ()) -> IO ()
initProperty logger = writeIORef props (logger, Map.empty)


termProperty :: IO ()
termProperty = writeIORef props undefined



property :: CoreFuncName -> Constraint -> IO (PropReq Int)
property func c = do
    info <- getInfo
    let key = (func,c)
    (logger,i) <- readIORef props
    case Map.lookup key i of
        Just y -> return $ lift y
        Nothing -> do
            let true = conTrue info
            res <- fix logger true conAnd (compute info i) (Map.singleton key true)
            writeIORef props (logger, i `Map.union` res)
            return $ lift $ fromJust $ Map.lookup key res
    where
        compute :: Info -> (Map.Map Key Constraint) -> (Key -> IO Constraint) -> Key -> IO Constraint
        compute info done ask (func,c) = do
            let get func c = case Map.lookup (func,c) done of
                                 Just res -> return $ lift res
                                 Nothing -> liftM lift $ ask (func,c)
                CoreFunc _ [arg] expr = function info func

            res <- backs get (propLit $ expr :< c)
            return $ propCon info arg res

        lift x = propLit $ 0 :< x
