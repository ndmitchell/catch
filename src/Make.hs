
module Make(make) where

import Hite
import Core
import Convert.CoreHite

import Directory
import Time


-- you are a dependancy on yourself


data Cache = CacheDepends FilePath [String]
           | CacheHite FilePath Hite
           | CacheData FilePath [Data]
           | CacheFunc FilePath [Func]
           | CacheCore FilePath Core
           

make :: FilePath -> IO Hite
make file = do c <- return []
               (c,h) <- demandHite file c
               return h
       

type Cr a = FilePath -> [Cache] -> IO ([Cache], a)
type C    = FilePath -> [Cache] -> IO [Cache]

-- HITE



demandHite :: Cr Hite
demandHite file c = do c2 <- ensureHite
                       let res = [x | CacheHite s x <- c2, s == file]
                       if null res
                           then do
                               h <- readCacheHite (file ++ ".hite")
                               return (c2, h)
                           else
                               return (c2, head res)


ensureHite :: C
ensureHite file c = do (c2,d) <- demandDeps file c
                       datHit <- fileDate file_hite
                       datDep <- maxFileDates d
                       if datHit > datDep then do
                            (c3,ds) <- mapC demandData dep c2
                            (c4,fs) <- mapC demandFunc dep c3
                            putStrLn $ "- Building hite for " ++ file
                            let h = reachable "" $ Hite (concat ds) (concat fs)
                            writeCacheHite h file_hite
                            return $ CacheHite file h : c
                        else
                            return c



{-


getHite :: FilePath -> [Cache] -> IO ([Cache], Hite)
getHite file c =
    case [x | CacheHite s x <- c, s == file] of
        [x] -> return (c, x)
        _ -> do
            (c, d) <- getCDep file c
            datDep <- depDate d
            datHit <- fileDate file_hite
            if datHit > datDep then buildHite d c else loadHite c
    where
        file_hite = file ++ ".hite"
    
        loadHite c = do h <- readCacheHite file_hite
                        return (CacheHite file h : c, h)
        
        buildHite dep c = do 
-}


mapC :: (FilePath -> [Cache] -> IO ([Cache], a)) -> [FilePath] -> [Cache] -> IO ([Cache], [a])
mapC f [] c = return (c, [])
mapC f (x:xs) c = do (c,r) <- f x c
                     (c,rs) <- mapC f xs c
                     return (c, r:rs)



demandData :: FilePath -> [Cache] -> IO ([Cache], [Data])
demandData file c = return (c, [])


demandFunc :: FilePath -> [Cache] -> IO ([Cache], [Func])
demandFunc file c = return (c, [])



demandDep :: FilePath -> [Cache] -> IO ([Cache], [FilePath])
demandDep file c = return (c, []) -- do src <- fileDate file



demandDeps :: FilePath -> [Cache] -> IO ([Cache], [FilePath])
demandDeps file c = do (c,d) <- demandDep file c
                       (c,ds) <- mapC demandDeps d c



fileDate :: FilePath -> IO ClockTime
fileDate file = do b <- doesFileExist file
                   if b then getModificationTime file else getClockTime


maximumFileDates :: [FilePath] -> IO ClockTime
maximumFileDates files = do x <- mapM getModificationTime files
                            return $ maximum x
