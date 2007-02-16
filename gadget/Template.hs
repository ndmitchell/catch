
module Template(Template, templateInit, templateGet) where

import Req
import Reduce
import Fixp
import System.IO
import Safe
import General
import Data.Proposition
import Data.IORef
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad
import Yhc.Core


type Cache = [((CoreFuncName,Val), Vals)]

data Template = Template Core Handle (IORef Cache)


templateInit :: Core -> Handle -> IO Template
templateInit core hndl = do
    x <- newIORef []
    return $ Template core hndl x


-- first element of Req must be a Call
templateGet :: Template -> Req -> IO Req
templateGet template@(Template core hndl cache) orig@(Req (CoreApp (CoreFun name) [arg]) vars) = do
    -- first check which elements are in the cache, and which aren't
    c <- readIORef cache
    let reqs = map (\v -> (name,v)) vars
        res = map (`lookup` c) reqs
    if all isJust res then return $ Req arg (valsOrs core $ map fromJust res) else do
        let new = [a | (Nothing,a) <- zip res reqs]
        hPutStrLn hndl $ "Templating:"
        mapM_ (hPutStrLn hndl . ("  "++) . show) new
        c2 <- templateAdd core c new
        writeIORef cache (c2 ++ c)
        hPutStrLn hndl $ "Added:"
        mapM_ (hPutStrLn hndl . ("  "++) . show) c2
        templateGet template orig -- should succeed straight away



templateAdd :: Core -> Cache -> [(CoreFuncName,Val)] -> IO [((CoreFuncName,Val), Vals)]
templateAdd core globalCache req =
        f [] (zip req (repeat valsTrue))
    where
        f done [] = return done
        f done todo@((key,val):rest) = do
            --putStrLn $ "Trying with " ++ show (length done) ++ ", " ++ show key ++ " = " ++ show val
            (new,val2) <- solve (done ++ todo ++ globalCache) key
            --when (not $ null new) $ putStrLn $ "ADDING " ++ show (length new) ++ " onto " ++ show (length todo + length done)
            let val3 = valsAnd core val val2
            if not $ null new then f [] (zip new (repeat valsTrue) ++ (key,val3) : rest ++ done)
             else if val3 /= val then f [] ((key,val3) : rest ++ done)
             else f ((key,val):done) rest


        solve :: Cache -> (CoreFuncName,Val) -> IO ([(CoreFuncName,Val)], Vals)
        solve cache req = do
            ref <- newIORef []
            res <- liftM (collapse core) $ reduceWithM core (g cache ref) $ instantiate core req
            new <- readIORef ref
            return (new, res)
        
        
        g :: Cache -> IORef [(CoreFuncName,Val)] -> Req -> IO Reqs
        g cache ref (Req (CoreApp (CoreFun name) [arg]) vars) = do
            let res = [(item, lookup item cache) | v <- vars, let item = (name,v)]
                (success,failure) = partition (isJust . snd) res
                ans = Req arg (valsOrs core $ map (fromJust . snd) success)
            when (not $ null failure) $ modifyIORef ref (\old -> snub $ old ++ map fst failure)
            return $ if null success then propTrue else propLit ans


-- given an expression (which will be a call) instantiate it by doing
-- beta reduction
instantiate :: Core -> (CoreFuncName,Val) -> Req
instantiate core (name,val) = res
    where
        res = Req (replaceFreeVars [(arg,CoreVar "?")] body) [val]
        CoreFunc _ [arg] body = coreFunc core name





{-

templateCache :: Template -> ReqCall -> IO Vals
templateCache template@(Template core hndl cache) key = do
    res <- readIORef cache
    case lookup key res of
        Just x -> return x
        Nothing -> do
            ans <- templateCalc template key
            modifyIORef cache ((key,ans):)
            hPutStrLn hndl $ "Add: " ++ show key ++ " = " ++ show ans
            return ans


templateCalc :: Template -> ReqCall -> IO Vals
templateCalc template@(Template core hndl _) req@(parent,_) = do
        hPutStrLn hndl $ "BEGIN: templateCalc, " ++ show req
        res <- fixpVals core [Any] f req
        hPutStrLn hndl $ "END  : templateCalc, " ++ show res
        return res
    where
        one = collapse core
    
        f :: ReqCall -> (ReqCall -> IO Vals) -> IO Vals
        f req gen = liftM one $ reduceWithM core (liftM propLit . g gen) $ instantiate core req
        
        g :: (ReqCall -> IO Vals) -> Req -> IO Req
        g gen req@(Req (CoreApp (CoreFun name) [arg]) vals)
            | parent `notElem` reachSet core name =
                templateGet template req
            
            | otherwise = do
                ans <- gen (name,vals)
                return $ Req arg ans


-- given an expression (which will be a call) instantiate it by doing
-- beta reduction
instantiate :: Core -> ReqCall -> Req
instantiate core (name,vals) = res
    where
        res = Req (replaceFreeVars [(arg,CoreVar "?")] body) vals
        CoreFunc _ [arg] body = coreFunc core name


reachSet :: Core -> CoreFuncName -> [CoreFuncName]
reachSet hill name = fixSet f [name]
    where
        f :: CoreFuncName -> [CoreFuncName]
        f x = nub [y | CoreApp (CoreFun y) _ <- allCore body, not $ "." `isPrefixOf` y]
            where CoreFunc _ _ body = coreFunc hill x
-}
