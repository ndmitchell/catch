
module Make(make, Depend(..)) where

import System
import List
import Directory
import Monad
import General.General


import qualified General.Make
import General.Make (Depend(..))

import Hite
import Core
import Convert.CoreHite





{-


data Depend dat = Depend
    FilePath -- ^ This file
    [FilePath] -- ^ The files I depend on
    (FilePath -> IO dat) -- ^ How to read a cache item
    (FilePath -> dat -> IO ()) -- ^ How to write a cache item
    ((FilePath -> dat) -> FilePath -> IO dat) -- ^ How to create a piece of data

-}


data MakeData = MdCore {fromMdCore :: Core}
              | MdHite {fromMdHite :: Hite}
              deriving Show


make :: FilePath -> IO Hite
make x = do ensureDirectory "Cache"
            ensureDirectory "Cache/Library"
            ensureDirectory "Cache/Example"
            src <- getFilePath x
            deps <- calcDeps src
            let alldeps = nub $ src : concatMap snd deps
                datas = map (\(x,_) -> Depend ("Cache/" ++ x ++ ".data") ["Cache/" ++ x ++ ".core"] rdCore wrCore (crData x)) deps
                parts = map (\(x,d) -> Depend ("Cache/" ++ x ++ ".part") (("Cache/" ++ x ++ ".core") : map (\y -> "Cache/" ++ y ++ ".data") d) rdHite wrHite (crPart x d)) deps
                final = Depend ("Cache/" ++ src ++ ".hite") (map (\x -> "Cache/" ++ x ++ ".part") alldeps) rdHite wrHite (crHite alldeps)
                exist = map (\(x,_) -> Depend ("Cache/" ++ x ++ ".core") [] rdCore (error "wrCore, no implementation") (error "crCore, no implementation")) deps
            
            MdHite res <- General.Make.make (final:datas++parts++exist) ("Cache/" ++ src ++ ".hite")
            return res
    where
        crData file get = do let MdCore (Core name items) = get $ "Cache/" ++ file ++ ".core"
                             return $ MdCore $ Core name $ filter isCoreData items
            
        crPart file dep get = return $ MdHite $ Hite (filter validData datas) funcs
            where
                Hite datas funcs = coreHite res
                validData dat = not (dataName dat `elem` addDatas)
                addDatas = [name | Core _ x <- cores, CoreData name _ <- x]
                res = Core name (concatMap (\(Core _ x) -> x) cores ++ dat)
                (Core name dat) = fromMdCore $ get $ "Cache/" ++ file ++ ".core"
                cores = map (\x -> fromMdCore $ get $ "Cache/" ++ x ++ ".data") dep
            
        rdHite src = readCacheHite src >>= return . MdHite
        wrHite src (MdHite val) = writeCacheHite val src
        
        crHite deps get = return $ MdHite $ reachable "" $ Hite (concatMap datas res) (concatMap insertMain $ concatMap funcs res)
            where res = map (\x -> fromMdHite $ get $ "Cache/" ++ x ++ ".part") deps
            
        rdCore src = readFile src >>= return . MdCore . readCore
        wrCore src (MdCore val) = writeFile src (show val)
        
        
        insertMain func@(Func name args body pos) | ".main" `isSuffixOf` name =
            [Func "main" args (Call (CallFunc name) (map Var args)) pos, func]
        insertMain func = [func]
            



-- * Figure out which file it is that you require

isPreamble :: FilePath -> Bool
isPreamble file = "Preamble.hs" `isSuffixOf` file


getFilePath :: FilePath -> IO FilePath
getFilePath file = do let fil = if file == "Prelude" then "Library/Preamble.hs"
                                else file ++ (if '.' `elem` file then "" else ".hs")
                          f1 = "Example/" ++ fil
                          f2 = "Library/" ++ fil
                      b1 <- doesFileExist f1
                      b2 <- doesFileExist f2
                   
                      if b1 then return f1
                       else if b2 then return f2
                       else error $ "Could not find file, " ++ file


-- * Dependancy tracking, through import statements

calcDeps :: FilePath -> IO [(FilePath, [FilePath])]
calcDeps file = f [file] []
    where
        f [] res = return res
        f (x:xs) res = do dep <- getDeps x
                          let newreq = (dep \\ map fst res) \\ (x:xs)
                          f (newreq++xs) ((x,dep):res)


getDeps :: FilePath -> IO [FilePath]
getDeps file = do keep_core <- newer cfile file
                  keep_dep  <- newer dfile file
                  when (not keep_core) buildCore
                  res <- if not (keep_core && keep_dep) then buildDep else loadDep
                  mapM getFilePath res
    where
        cfile = "Cache/" ++ file ++ ".core"
        dfile = "Cache/" ++ file ++ ".dep"
        
        loadDep = readFile dfile >>= return . lines
        
        
        buildCore :: IO ()
        buildCore = do
            let file2 = "Cache/" ++ file
                preamble = isPreamble file
                out = cfile ++ "2"
                cmd = "yhc " ++ file2 ++ " -dst Cache -hidst Cache -corep 2> " ++ out
                
            putStrLn $ "MAKE " ++ cfile
            putStrLn $ "     " ++ cmd
            copyFile file file2
            code <- system cmd
            
            when (code /= ExitSuccess) $ do
                res <- readFile out
                putStrLn $ "Failed to compile\n" ++ res
                error "Fix compilation errors"
            
            b <- doesFileExist out
            when (not b) $ error "Core file not generated, do you have Yhc installed?"
            
            res <- readFile out
            writeFile cfile ((if preamble then stripPreamble else id) res)
            

        buildDep = do src <- readFile cfile
                      let ans = if isPreamble file then [] else ("Preamble":depends (readCore src))
                      writeFile dfile $ unlines ans
                      return ans


copyFile src dest = readFile src >>= writeFile dest
                      


-- | is the first file newer than the second
--   if either does not exist, the answer is false
newer :: FilePath -> FilePath -> IO Bool
newer f1 f2 = do b1 <- doesFileExist f1
                 b2 <- doesFileExist f2
                 if b1 && b2 then
                    do m1 <- getModificationTime f1
                       m2 <- getModificationTime f2
                       return $ m1 > m2
                  else return False




stripPreamble src = strip src                             
    where
        tupStart = ",CoreFunc (CoreApp (CoreVar \"Preamble.tup"
        tupEnd = ")]))"

        strip x | tupStart `isPrefixOf` x = strip (dropTup x)
        strip (x:xs) = x : strip xs
        strip [] = []


        dropTup x | tupEnd `isPrefixOf` x = drop (length tupEnd) x
        dropTup (x:xs) = dropTup xs



