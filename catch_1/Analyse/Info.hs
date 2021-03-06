
module Analyse.Info(
    initInfo, termInfo, getInfo,
    CoreField, Info,
    ctors, arity, var, function, isRec,
    coreAlts
    ) where

import Data.IORef
import Data.Maybe
import Data.List
import Yhc.Core
import Foreign
import qualified Data.Map as Map
import qualified Data.Set as Set

type CoreField = (CoreCtorName, Int)


data Info = Info
    {ctors        :: CoreCtorName  -> [CoreCtorName]
    ,arity        :: CoreCtorName  -> Int
    ,var          :: CoreVarName   -> Maybe (CoreExpr, CoreField)
    ,function     :: CoreFuncName  -> CoreFunc
    ,isRec        :: CoreField     -> Bool
    }


info :: IORef Info
info = unsafePerformIO $ newIORef undefined


getInfo :: IO Info
getInfo = readIORef info


termInfo :: IO ()
termInfo = writeIORef info undefined


initInfo :: Core -> IO ()
initInfo core = writeIORef info res
    where
        res = Info
            (\x -> fromJust $ Map.lookup x ctors_)
            (\x -> fromJust $ Map.lookup x arity_)
            (\x -> Map.lookup x vars_)
            (coreFuncMap funcMap)
            (\x -> x `Set.member` recs_)
    
        ctors_ = Map.fromList [(c,cs) | d <- coreDatas core, let cs = sort $ map coreCtorName $ coreDataCtors d, c <- cs]
        arity_ = Map.fromList [(coreCtorName c,length (coreCtorFields c)) | d <- coreDatas core, c <- coreDataCtors d]
        vars_  = Map.fromList $ concat [concatMap (pickVars on . fst) alts | CoreCase on alts <- allCore core]

        recs_ = Set.fromList $ concatMap recursiveFields $ coreDatas core

        funcMap = toCoreFuncMap core



recursiveFields :: CoreData -> [CoreField]
recursiveFields dat = [(coreCtorName ctr, i)
                      | ctr <- coreDataCtors dat, (i,(typ,_)) <- zip [0..] (coreCtorFields ctr)
                      , rec == filter (`notElem` "()") typ]
    where
        rec = unwords $ coreDataName dat : coreDataTypes dat


pickVars :: CoreExpr -> CoreExpr -> [(CoreVarName, (CoreExpr, CoreField))]
pickVars on (CoreApp (CoreCon name) args) = [(n, (on, (name, i))) | (i, CoreVar n) <- zip [0..] args]
pickVars _ _ = []



coreAlts :: [(CoreExpr, CoreExpr)] -> IO [([CoreCtorName], CoreExpr)]
coreAlts alts = do
        info <- getInfo
        let seen = [getCtor a | (a,b) <- alts, not $ isCoreVar a]
            cs = ctors info (head seen)
        return [(if isCoreVar a then cs \\ seen else [getCtor a], b) | (a,b) <- alts]
    where
        getCtor (CoreCon c) = c
        getCtor (CoreApp x _) = getCtor x

