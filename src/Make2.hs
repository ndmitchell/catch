
module Make2(make2) where

import System
import List
import Directory
import Monad
import Data.Char
import General.General

import Hite
import Core
import Convert.CoreHite



-- take a haskell file (probably in the example directory)
-- to the end result Hite
make2:: FilePath -> IO Hite
make2 x = do
        ensureDirectory "Cache"
        ensureDirectory "Cache/Library"
        ensureDirectory "Cache/Example"
        
        src <- getFilePath x
        system $ "yhc \"" ++ src ++ "\" -corep -dst Cache/Example -hidst Cache/Example"
        
        -- now do a transitive closure on the depandancies
        deps@((_,cache):_) <- collectDeps x
        dirty <- anyM isDirty deps
        let newcache = cache ++ ".hite"
        b <- doesFileExist newcache
        if not dirty && b then readCacheHite (cache ++ ".hite") else do
            datas <- mapM ensureData deps
            let dat = injectData $ mergeHite datas
            codes <- mapM (ensureCode dat) deps
            putStrLn $ "Creating " ++ newcache
            let hite = mergeHites [reachable "" $ injectCode $ insertMain $ mergeHite (dat:codes)]
            writeCacheHite hite (cache ++ ".hite")
            return hite
    where
        anyM f [] = return False
        anyM f (x:xs) = f x >>= \b -> if b then return True else anyM f xs
        
    
        -- (requires-data, requires-code)
        isDirty (corefile,cache) = do
            b1 <- testDirty corefile (cache ++ ".data")
            b2 <- testDirty corefile (cache ++ ".code")
            return $ b1 || b2

        testDirty src cache = do
            b1 <- doesFileExist cache
            if not b1 then return True else do
                m0 <- getModificationTime src
                m1 <- getModificationTime cache
                return $ m1 < m0
        
        ensureData (corefile,cache) = do
            let newcache = cache ++ ".data"
            b <- testDirty corefile newcache
            if not b then readCacheHite newcache else do
                putStrLn $ "Creating: " ++ newcache
                items <- coreItems corefile
                let hite = coreDatas $ takeWhile isCoreData items
                writeCacheHite hite newcache
                return hite

        ensureCode datas (corefile,cache) = do
            let newcache = cache ++ ".code"
            b <- testDirty corefile newcache
            if not b then readCacheHite newcache else do
                putStrLn $ "Creating: " ++ newcache
                items <- coreItems corefile
                let hite = coreFuncs datas $ dropWhile isCoreData items
                writeCacheHite hite newcache
                return hite
        
        
        mergeHite :: [Hite] -> Hite
        mergeHite xs = Hite (concatMap datas xs) (concatMap funcs xs)


        insertMain (Hite a b) = Hite a (concatMap f b)
            where
                f func@(Func name args body pos) | ".main" `isSuffixOf` name =
                    [Func "main" args (Call (CallFunc name) (map Var args)) pos, func]
                f func = [func]
        
        
        injectData (Hite a b) = Hite (special:a) b
            where
                special = Data "Prelude.1()" [Ctor "Prelude.1()" ["tup1_1"] [TyFree "b"]] ["b"]
        
        injectCode (Hite a b) = Hite a (special++b)
            where
                primOne = ["prim_ORD"]
                primTwo = ["prim_EQ_W","prim_GT_W","prim_ADD_W","prim_LE_W","prim_LT_W",
                           "prim_SUB_W","prim_GE_W","prim_NE_W","prim_MUL_W","prim_NEG_W",
                           "prim_REM","prim_QUOT","prim_SEQ"]

                special = [Func f1 ["v1"] (Prim f1 [Var "v1"]) "<internal>" | f1 <- primOne] ++
                          [Func f2 ["v1","v2"] (Prim f2 [Var "v1",Var "v2"]) "<internal>" | f2 <- primTwo]
        
        {-
        
            deps <- calcDeps src
            let alldeps = nub $ src : concatMap snd deps
                datas = map (\(x,_) -> Depend ("Cache/" ++ x ++ ".data") ["Cache/" ++ x ++ ".core"] rdCore wrCore (crData x)) deps
                parts = map (\(x,d) -> Depend ("Cache/" ++ x ++ ".part") (("Cache/" ++ x ++ ".core") : map (\y -> "Cache/" ++ y ++ ".data") d) rdHite wrHite (crPart x d)) deps
                final = Depend ("Cache/" ++ src ++ ".hite") (map (\x -> "Cache/" ++ x ++ ".part") alldeps) rdHite wrHite (crHite alldeps)
                exist = map (\(x,_) -> Depend ("Cache/" ++ x ++ ".core") [] rdCore (error "wrCore, no implementation") (error "crCore, no implementation")) deps
            
            MdHite res <- General.Make.make (final:datas++parts++exist) ("Cache/" ++ src ++ ".hite")
            return res
    where
        crData file get = do let MdCore (Core name dep items) = get $ "Cache/" ++ file ++ ".core"
                             return $ MdCore $ Core name dep $ filter isCoreData items
            
        crPart file dep get = return $ MdHite $ Hite (filter validData datas) funcs
            where
                Hite datas funcs = coreHite res
                validData dat = not (dataName dat `elem` addDatas)
                addDatas = [name | Core _ _ x <- cores, CoreData name _ _ <- x]
                res = Core name [] (concatMap (\(Core _ _ x) -> x) cores ++ dat)
                (Core name _ dat) = fromMdCore $ get $ "Cache/" ++ file ++ ".core"
                cores = map (\x -> fromMdCore $ get $ "Cache/" ++ x ++ ".data") dep
            
        rdHite src = readCacheHite src >>= return . MdHite
        wrHite src (MdHite val) = writeCacheHite val src
        
        crHite deps get = return $ MdHite $ reachable "" $ Hite ds (concatMap insertMain fs)
            where
                res = map (\x -> fromMdHite $ get $ "Cache/" ++ x ++ ".part") deps
                Hite ds fs = mergeHites res
            
        rdCore src = readFile src >>= return . MdCore . readCore
        wrCore src (MdCore val) = writeFile src (show val)
        
        

-}

coreItems :: FilePath -> IO [CoreItem]
coreItems corefile = do
    src <- readFile corefile
    return $ concatMap f $ tail $ lines src
    where
        f "]" = []
        f x = [readNote "coreItems" $ tail $ dropWhile isSpace x]





-- (core-location, cache-location)
collectDeps :: String -> IO [(FilePath, String)]
collectDeps x = do
        base <- getEnv "YHC_BASE_PATH"
        (file,cache) <- locateFile base x
        deps <- getDeps file
        res <- f base deps []
        return $ (file,cache) : res
    where
        f base [] done = return []
        f base (t:odo) done | t == "PreludeBuiltin" || t `elem` done = f base odo done
        f base (t:odo) done = do
            (file,cache) <- locateFile base t
            deps <- getDeps file
            res <- f base (deps++odo) (t:done)
            return $ (file,cache) : res


getDeps :: FilePath -> IO [String]
getDeps x = do
    src <- readFile x
    let (h:t) = lines src
    let sec = dropWhile (/= '[') h
        sec2 = if null t then take (length sec - 3) sec else sec
    return $ "YHC.Internal" : readNote "getDeps" sec2


-- from a module, find the FilePath
locateFile :: FilePath -> String -> IO (FilePath, String)
locateFile base file = do
    let f1 = "Cache/Example/" ++ file ++ ".ycr"
        f2 = base ++ "/lib/yhc/packages/yhc-base/1.0/" ++ map g file ++ ".ycr"
    b1 <- doesFileExist f1
    b2 <- doesFileExist f2
    if b1 then return (f1, "Cache/Example/" ++ file)
     else if b2 then return (f2, "Cache/Library/" ++ file)
     else error $ "Could not find module, " ++ file ++ "\n\tLooked at:\t" ++ f1 ++ "\n\t" ++ f2
    where
        g x = if x == '.' then '/' else x



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


{-

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
                      
-}

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

{-


stripPreamble src = strip src                             
    where
        tupStart = ",CoreFunc (CoreApp (CoreVar \"Preamble.tup"
        tupEnd = ")]))"

        strip x | tupStart `isPrefixOf` x = strip (dropTup x)
        strip (x:xs) = x : strip xs
        strip [] = []


        dropTup x | tupEnd `isPrefixOf` x = drop (length tupEnd) x
        dropTup (x:xs) = dropTup xs



-}
