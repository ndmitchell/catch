
module Reduce(reduce, reduces, reduceWithM, reducesWithM, propMapReduceM, propMapReduce) where

import Req
import General
import Data.Proposition
import Control.Monad
import Control.Monad.Identity
import Yhc.Core
import Data.List
import Data.Maybe


-- | Should have no negate done first
reduces :: Formula Req -> Formula Req
reduces reqs = propMap reduce reqs


reduce :: Req -> Formula Req
reduce req@(Req hill expr path ctors) = case expr of
    CoreApp (CoreFun x) _ | not $ "." `isPrefixOf` x -> propLit req
    CoreVar x | '.' `notElem` x -> propLit req
    _ -> reduces $ reduceOne req
reduce x = propLit x


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Formula Req
reduceOne req@(Req hill expr path ctors) = case expr of
    CoreApp (CoreFun ('.':y)) [x] -> newReqs hill x (path `integrate` y) ctors
    
    CoreApp (CoreCon y) xs -> propAnds (p1:ps)
        where
            cargs = map (fromJust . snd) $ coreCtorFields $ coreCtor hill y

            p1 = if ewpPath path then propBool (y `elem` ctors) else propTrue
            ps = zipWithEq f xs cargs
            
            f x carg = case path `differentiate` carg of
                           Nothing -> propTrue
                           Just path2 -> newReqs hill x path2 ctors
    
    CoreCase on alts -> propAnds $ map f alts
        where
            allCtrs = ctorNames $ coreCtorData hill $ fromCoreCon $ fst $ head alts
            seenCtrs = [x | (CoreCon x, _) <- alts]

            f (CoreCon ctr, rhs) = g (delete ctr allCtrs) rhs
            f (CoreVar _, rhs) = g seenCtrs rhs
            
            g ctrs ex = newReqs hill on (emptyPath hill) ctrs `propOr` newReqs hill ex path ctors

    CoreApp (CorePrim "error") _ -> propLit Angelic -- since will never return anything
    CoreApp (CorePrim x) ys -> propLit Demonic -- absolutely no idea what the result is
    c | isCoreConst c -> propLit Demonic -- if you care, abstract before here

    _ -> error $ "reduceOne: " ++ show req
    


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: (Req -> IO (Formula Req)) -> Formula Req -> IO (Formula Req)
reducesWithM f reqs = propMapReduceM (reduceWithM f) reqs


reduceWithM :: (Req -> IO (Formula Req)) -> Req -> IO (Formula Req)
reduceWithM f req@(Req hill expr path ctors) = case expr of
    CoreApp (CoreFun _) _ -> f req >>= reducesWithM f
    CoreVar x | '.' `notElem` x -> return $ propLit req
    _ -> reducesWithM f $ reduceOne req
reduceWithM f x = return $ propLit x


propMapReduceM :: Monad m => (Req -> m (Formula Req)) -> Formula Req -> m (Formula Req)
propMapReduceM f x = propMapM (liftM reduces . f) x

propMapReduce :: (Req -> Formula Req) -> Formula Req -> Formula Req
propMapReduce f x = runIdentity $ propMapReduceM (return . f) x



splitVar :: String -> (String, [String])
splitVar xs = (y, ys)
    where (y:ys) = words [if x == '.' then ' ' else x | x <- xs]
