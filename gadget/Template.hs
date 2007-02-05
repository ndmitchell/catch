
module Template(Template, templateInit, templateGet) where

import Req
import Reduce
import Fixp
import System.IO
import Safe
import General
import Data.Proposition
import Data.IORef
import Data.Char
import Data.List
import Control.Monad
import Yhc.Core


data Template = Template Core Handle (IORef [(ReqCall, Vals)])


templateInit :: Core -> Handle -> IO Template
templateInit core hndl = do
    x <- newIORef []
    return $ Template core hndl x


-- first element of Req must be a Call
templateGet :: Template -> Req -> IO Req
templateGet template (Req (CoreApp (CoreFun name) [arg]) vars) = do
    res <- templateCache template (name,vars)
    return $ Req arg res


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
