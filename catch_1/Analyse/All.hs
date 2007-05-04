
module Analyse.All(analyse) where

import Yhc.Core
import Analyse.Info
import Analyse.Property
import Analyse.Precond
import Control.Monad.State
import General.CmdLine


-- given a logger and the core, do the work
-- with logger, True = precondition, False = property
-- should you split up the errors separately, True=yes, False=no
-- should you do partial function listined
analyse :: (Bool -> String -> IO ()) -> [Option] -> Core -> IO String
analyse logger options core = do
    let split = Errors `elem` options
        partials = Partial `elem` options
        (msgs,core2) = if split then labelErrors core else ([],core)
        funcs = map coreFuncName $ filter isCoreFunc $ coreFuncs core2
    
    initInfo core2
    initProperty (logger False)
    
    res <- if split then do
               res <- preconds (logger True) partials msgs funcs
               putStrLn "Checking whole program"
               return res
           else do
               putStrLn "Checking whole program"
               precond (logger True) partials (const True) funcs

    termInfo
    termProperty
    return $ show res



-- label the errors starting at 0
labelErrors :: Core -> ([String],Core)
labelErrors core = (reverse res,core2)
    where
        (core2,(n,res)) = runState (mapUnderCoreM f core) (0,[])
        
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
        g n _ = "Unknown"

