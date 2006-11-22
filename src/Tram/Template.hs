
module Tram.Template(Template, templateInit, templateGet) where

import Tram.Req
import Tram.Reduce
import Tram.Fixp
import System.IO
import General.General
import Data.Proposition
import Data.IORef
import Data.Char
import Data.List
import Control.Monad
import Hill.All


data Template = Template Hill Handle (IORef [(Req, Formula Req)])


templateInit :: Hill -> Handle -> IO Template
templateInit hill hndl = do
    x <- newIORef []
    return $ Template hill hndl x


-- first element of Req must be a Call
templateGet :: Template -> Req -> IO (Formula Req)
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
    return $ propRebuild $ templateConcrete req ans



-- need to make it more abstract, and then more concrete, to satisfy the cache
templateAbstract :: Req -> Req
templateAbstract (Req a (Call name xs) b c) = newReq a (Call name args) b c
    where args = map Var [0..length xs-1]


templateConcrete :: Req -> Formula Req -> Formula Req
templateConcrete (Req _ (Call name args) _ _) y = propMapReduce (propLit . f) y
    where
        f (Req a b c d) = newReq a (mapOver g b) c d
        g (Var i) = args !! i
        g x = x


-- expr is Call
templateCalc :: Template -> Hill -> Handle -> Req -> IO (Formula Req)
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


instantiate :: Hill -> Req -> Formula Req
instantiate (Hill datas funcs) r1@(Req a (Call name args) b c) = res
    where
        res = propMapReduce rep $ newReqs a body b c
    
        (args2, body) = head [(a,b) | Func nam a b <- funcs, nam == name]
        
        rep (Req a b c d) = newReqs a (mapOver g b) c d
        
        g (Var x) = case lookup x (zip args2 args) of
                          Nothing -> error $ "Tram.Template.instantiate: not found" -- Var x
                          Just y -> y
        g x = x



getFuncName :: Req -> FuncName
getFuncName (Req _ (Call nam _) _ _) = nam


reachSet :: Hill -> FuncName -> [FuncName]
reachSet hill name = fixSet f [name]
    where
        f :: FuncName -> [FuncName]
        f x = nub [y | Call y _ <- allOver body]
            where Func _ _ body = getFunc hill x
