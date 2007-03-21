
module Analyse.All(analyse) where

import Yhc.Core
import Analyse.Info
import Analyse.Property
import Analyse.Precond
import Control.Monad.State


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
-- should you split up the errors separately, True=yes, False=no
analyse :: (Bool -> String -> IO ()) -> Bool -> Core -> IO String
analyse logger split core = do
    let (msgs,core2) = if split then labelErrors core else ([],core)
        funcs = map coreFuncName $ coreFuncs core2
    
    initInfo core2
    initProperty (logger False)
    
    res <- if split then do
               res <- preconds (logger True) msgs funcs
               putStrLn "Checking whole program"
               return res
           else do
               putStrLn "Checking whole program"
               precond (logger True) (const True) funcs

    res <- precond (logger True) (const True) (map coreFuncName $ coreFuncs core)
    termInfo
    termProperty
    return $ show res



-- label the errors starting at 0
labelErrors :: Core -> ([String],Core)
labelErrors core = (reverse res,core2)
    where
        (core2,(n,res)) = runState (mapUnderCoreM f core) (0,[])
        
        f (CoreApp (CorePrim "Prelude.error") (x:_)) = do
            (n,xs) <- get
            put (n+1,g 10 x:xs)
            return $ CoreApp (CorePrim "Prelude.error") [CoreInt n]
        f x = return x
        
        g n _ | n <= 0 = "Unknown"
        g n (CoreStr x) = x
        g n (CoreApp (CoreCon _) (x:_)) = g (n-1) x
        g n (CoreApp (CoreFun x) _) = g (n-1) $ coreFuncBody $ coreFunc core x
        g n _ = "Unknown"

