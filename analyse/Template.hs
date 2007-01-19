
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


data Template = Template Core Handle (IORef [(Req, Reqs)])


templateInit :: Core -> Handle -> IO Template
templateInit hill hndl = do
    x <- newIORef []
    return $ Template hill hndl x


-- first element of Req must be a Call
templateGet :: Template -> Req -> IO Reqs
templateGet template@(Template hill hndl cache) req = do
    let abstract = templateAbstract req
    res <- readIORef cache
    ans <- case lookup abstract res of
               Just x -> return x
               Nothing -> do
                   ans <- templateCalc template hill hndl abstract
                   modifyIORef cache ((abstract,ans):)
                   hPutStrLn hndl $ "Add: " ++ show abstract ++ " = " ++ show ans
                   return ans
    return $ templateConcrete req ans



-- need to make it more abstract, and then more concrete, to satisfy the cache
templateAbstract :: Req -> Req
templateAbstract (Req (CoreApp (CoreFun name) xs) (PathCtor a b c)) = newReq a (CoreApp (CoreFun name) args) b c
    where args = [CoreVar $ 'v':show i | i <- [0..length xs-1]]


templateConcrete :: Req -> Reqs -> Reqs
templateConcrete (Req (CoreApp (CoreFun name) args) _) y = propMapReduce (propLit . f) y
    where
        nargs = length args
        f (Req b (PathCtor a c d)) = newReq a (mapUnderCore g b) c d
        g (CoreVar ('v':si)) | i < nargs = args !! i where i = readNote ("templateConcrete, " ++ si) si
        g x = x


-- expr is Call
templateCalc :: Template -> Core -> Handle -> Req -> IO Reqs
templateCalc template hill hndl req = do
        hPutStrLn hndl $ "BEGIN: templateCalc, " ++ show req
        res <- liftM propSimplify $ fixpReqs propTrue f req
        hPutStrLn hndl $ "END  : templateCalc, " ++ show res
        return $ propSimplify res
    where
        parent = getFuncName req

        f req gen = do
            let reqs = instantiate hill req
            reducesWithM (g gen) reqs

        -- can do it more efficiently, a fixp cut
        g gen req | parent `notElem` reachSet hill (getFuncName req) =
            templateGet template req

        g gen req = do
            let abstract = templateAbstract req
            answer <- gen abstract
            let conc = templateConcrete req answer
            res <- liftM propSimplify $ reducesWithM (g gen) conc
            return res


instantiate :: Core -> Req -> Reqs
instantiate core r1@(Req (CoreApp (CoreFun name) args) (PathCtor a b c)) = res
    where
        res = propMapReduce rep $ newReqs a body b c
    
        CoreFunc _ args2 body = coreFunc core name
        
        rep (Req b (PathCtor a c d)) = newReqs a (mapUnderCore g b) c d
        
        g (CoreVar x) = case lookup x (zip args2 args) of
                          Nothing -> CoreVar x -- a let bound var
                          Just y -> y
        g x = x



getFuncName :: Req -> CoreFuncName
getFuncName (Req (CoreApp (CoreFun nam) _) _) = nam


reachSet :: Core -> CoreFuncName -> [CoreFuncName]
reachSet hill name = fixSet f [name]
    where
        f :: CoreFuncName -> [CoreFuncName]
        f x = nub [y | CoreApp (CoreFun y) _ <- allCore body, not $ "." `isPrefixOf` y]
            where CoreFunc _ _ body = coreFunc hill x
