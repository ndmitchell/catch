
module Analyse.All(analyse) where

import Yhc.Core
import Analyse.Info
import Analyse.Property
import Analyse.Precond
import Control.Monad.State
import General.CmdLine
import Analyse.Req
import Data.List
import Data.Maybe
import System.Timeout


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
-- should you split up the errors separately, True=yes, False=no
-- should you do partial function listined
analyse :: (Bool -> String -> IO ()) -> [Flag] -> Core -> IO String
analyse logger options core = do
    let quiet = Quiet `elem` options
        partials = not quiet
        (msgs,core2) = labelErrors core
        use = [1..length msgs] \\ concat [i | Skip i <- options]
        funcs = map coreFuncName $ filter isCoreFunc $ coreFuncs core2

        timer x = do
            let t = last $ defaultTimeout : [i | Timeout i <- options]
            res <- if t == 0 then liftM Just x else timeout (t * 1000000) x
            when (isNothing res) $ putStrLn "ERROR: Timeout occurred"
            return res

    initInfo core2{coreFuncs = CorePrim "any?" 0 : coreFuncs core2}
    initProperty (logger False)
    
    info <- getInfo
    res <- if quiet then do
               putStrLn "Checking whole program"
               res <- timer $ precond (logger True) partials use funcs
               return $ fromMaybe (conTrue info) res
           else do
               res <- flip mapM (zip [1..] msgs) $ \(i,msg) ->
                   let count = " [" ++ show i ++ "/" ++ show (length msgs) ++ "]: " ++ msg in
                   if i `notElem` use then do
                       putStrLn $ "Skipping" ++ count
                       return Nothing
                   else do
                       putStrLn $ "Checking" ++ count
                       res <- timer $ precond (logger True) partials [i] funcs
                       when (isJust res) $ putStrLn $ "Answer: " ++ show (fromJust res)
                       return res
               putStrLn "Checking whole program"
               return $ conAnds info $ catMaybes res

    termInfo
    termProperty
    return $ show res



-- label the errors starting at 1
labelErrors :: Core -> ([String],Core)
labelErrors core = (reverse res,core2)
    where
        (core2,(n,res)) = runState (mapUnderCoreM f core) (1,[])
        
        f (CoreApp (CoreFun "Prelude.error") (x:_)) = do
            (n,xs) <- get
            put (n+1,g 10 x:xs)
            return $ CoreApp (CoreFun "Prelude.error") [CoreInt n]
        f x = return x
        
        -- calculate the error message (has a termination bound built in)
        g n _ | n <= 0 = "Unknown"
        g n (CoreStr x) = x
        g n (CoreApp (CoreCon _) (x:_)) = g (n-1) x
        g n (CoreApp (CoreFun x) _) | isCoreFunc func = g (n-1) $ coreFuncBody func
            where func = coreFunc core x
        g n (CoreFun x) = g n (CoreApp (CoreFun x) [])
        g n _ = "Unknown"

