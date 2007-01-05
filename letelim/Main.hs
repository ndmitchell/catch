
module Main where

import System.FilePath
import Yhc.Core
import System.Environment
import System.Directory
import Data.List
import Control.Monad.State


main = getArgs >>= mapM_ f
    where
        f x = do
            x2 <- findFile x
            let x3 = dropExtensions x2 <.> "letelim.yca"
            core <- loadCore x2
            let core2 = letElim core
            saveCore x3 core2
            writeFile (x3 <.> "html") (coreHtml core2)
            print core2


findFile :: String -> IO FilePath
findFile file = do
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        (x:_) -> return x
        _ -> error $ "File not found, " ++ file
    where files = file : ["../examples" </> s </> "ycr" </> file <.> "first.yca" | s <- ["Example","Nofib"]]



letElim :: Core -> Core
letElim = letKill . letMove . letAdd . coreReachable ["main"] . coreInline InlineForward



letAdd :: Core -> Core
letAdd = applyBodyCore f
    where
        f x = evalState (mapUnderCoreM g x2) vars
            where
                vars = variableSupply 'v' \\ collectAllVars x2
                x2 = uniqueBoundVarsWithout (collectAllVars x) x

        g (CoreCase (CoreVar x) xs) = return $ CoreCase (CoreVar x) xs
        g (CoreCase x xs) = do
            (y:ys) <- get
            put ys
            return $ CoreLet [(y,x)] (CoreCase (CoreVar y) xs)
        g x = return x


-- Assume non-recursive lets
-- And entirely unique free variables
letMove :: Core -> Core
letMove = applyBodyCore (mapUnderCore f)
    where
        f (CoreLet bind1 (CoreLet bind2 x)) = coreLet (bind1++safe) (coreLet conflict x)
            where
                vars = map fst bind1
                (safe,conflict) = partition (isSafe . snd) bind2
                
                isSafe x = disjoint (collectFreeVars x) vars
                disjoint x y = length x == length (x \\ y)

        f x = x


letKill :: Core -> Core
letKill = id

