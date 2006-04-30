{- |
    Make module
    
    Given a list of dependancies, and cached files, figure out what needs
    to be recomputed and do it.
    
    A reasonably tricky problem!
-}
    
module General.Make(make, Depend(..)) where


import Directory
import Time
import Maybe


-- * Data structures

-- | Dependancy information
data Depend dat = Depend
    FilePath -- ^ This file
    [FilePath] -- ^ The files I depend on
    (FilePath -> IO dat) -- ^ How to read a cache item
    (FilePath -> dat -> IO ()) -- ^ How to write a cache item
    ((FilePath -> dat) -> IO dat) -- ^ How to create a piece of data


dependFile (Depend x _ _ _ _) = x
dependDeps (Depend _ x _ _ _) = x


data Cache dat = Cache [(FilePath, dat)]


-- * The main driver


-- | Create the data structure required by file, from the dependancy info
--   Assume no circular dependancies
make :: [Depend dat] -> FilePath -> IO dat
make deps file =
    do
        depends <- checkFileDates deps
        cache <- demandItem depends file (Cache [])
        return $ getItem cache file




getItem :: Cache dat -> FilePath -> dat
getItem (Cache xs) name = fromJust $ lookup name xs



-- * Item getting and creation

demandItems :: [ValidDepend dat] -> [FilePath] -> Cache dat -> IO (Cache dat)
demandItems deps []     cache= return cache
demandItems deps (f:fs) cache = do cache2 <- demandItem deps f cache
                                   demandItems deps fs cache2

demandItem :: [ValidDepend dat] -> FilePath -> Cache dat -> IO (Cache dat)
demandItem deps file (Cache cache) =
    do
        let items = filter ((==) file . dependFile . snd) deps
            [(valid, Depend _ depend rd wr cr)] = if null items then error $ "null items, when looking for, " ++ file
                                                  else items
        if valid then
            do value <- rd file
               return $ Cache $ (file,value):cache
         else
            do cache2 <- demandItems deps depend (Cache cache)
               value <- cr (getItem cache2)
               wr file value
               return $ Cache $ (file,value):cache



-- * Cache consistency checking

-- is a dependancy valid
type ValidDepend dat = (Bool, Depend dat)

{- For each cached item it is either valid or invalid
   It is valid iff:
      - it exists
      - its dependancies are valid
      - its modification date is after its dependancy

   Algorithm:
     get the modification date of each one, as Maybe ClockTime
     perform checks n+1 times (where n is the number of dependancies)
        [this guarantees a fixed point]
     convert the maybe to a bool
-}


checkFileDates :: [Depend dat] -> IO [ValidDepend dat]
checkFileDates x = do x2 <- mapM doesExist x
                      x3 <- return $ fixedpoint (length x + 1) x2
                      return [(isJust a, b) | (a,b) <- x3]
    where
        doesExist dep = do let file = dependFile dep
                           res <- doesFileExist file
                           if res
                               then do
                                   tim <- getModificationTime file
                                   return (Just tim, dep)
                               else return (Nothing, dep)

        fixedpoint 0 x = x
        fixedpoint n x = fixedpoint (n-1) $ map (iter x) x


        iter deps (valid, dep) = (if stillValid then valid else Nothing, dep)
            where
                reqs = [a | (a,x) <- deps, dependFile x `elem` dependDeps dep]
                stillValid = isJust valid &&
                             not (any isNothing reqs) &&
                             (null reqs || fromJust valid > maximum (map fromJust reqs))
