
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


props :: IORef (Map.Map Key Constraint)
props = unsafePerformIO undefined


initProperty :: IO ()
initProperty = writeIORef props Map.empty


termProperty :: IO ()
termProperty = writeIORef props undefined



property :: CoreFuncName -> Constraint -> IO (PropReq Int)
property func c = do
    info <- getInfo
    let key = (func,c)
    i <- readIORef props
    case Map.lookup key i of
        Just y -> return $ lift y
        Nothing -> do
            res <- fix conTrue conAnd (compute info i) (Map.singleton key conTrue)
            writeIORef props (i `Map.union` res)
            return $ lift $ fromJust $ Map.lookup key res
    where
        compute :: Info -> (Map.Map Key Constraint) -> (Key -> IO Constraint) -> Key -> IO Constraint
        compute info done ask (func,c) = do
            let get func c = liftM lift $ ask (func,c)
                expr = instantiate info func [CoreVar "?"]
            res <- backs get (propLit $ expr :< c)
            return $ propCon "?" res

        lift x = propLit $ 0 :< x
