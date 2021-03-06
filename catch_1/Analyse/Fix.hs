
module Analyse.Fix(fix) where

import Data.IORef
import Data.List
import Data.Maybe
import General.General
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set


-- dumb version of fix, does not do smart dependancies
fixDumb
    :: (Show v, Eq v, Show k, Ord k)
    => v                            -- default value
    -> (v -> v -> v)                -- combine
    -> ((k -> IO v) -> k -> IO v)   -- compute
    -> Map.Map k v                  -- initial
    -> IO (Map.Map k v)             -- result
fixDumb def combine compute initial = cont initial
    where
        cont x = do
            x2 <- step x
            if x == x2
                then return x
                else cont x2
    
        step x = do
            (vs,add) <- mapAndUnzipM g (Map.toAscList x)
            let mp1 = Map.fromAscList vs
                mp2 = Map.fromAscList $ zip (filter (not . (`Map.member` x)) $ snub $ concat add) (repeat def)
            return $ Map.union mp1 mp2
            where
                g (k,v) = do
                    (v2,add) <- execute (\q -> Map.findWithDefault def q x) compute k
                    return ((k, combine v v2), add)



-- does smart dependancies and has a heuristic for the best order

data Item k v = Item {value :: v, dependsOn :: Set.Set k, requiredBy :: Set.Set k}

fix
    :: (Show v, Eq v, Show k, Ord k)
    => (String -> IO ())            -- logger
    -> (v -> Bool)                  -- partial function listing
    -> v                            -- default value
    -> (v -> v -> v)                -- combine
    -> ((k -> IO v) -> k -> IO v)   -- compute
    -> Map.Map k v                  -- initial
    -> IO (Map.Map k v)             -- result
fix logger listing def combine compute initial = do
        logger "BEGIN FIXED POINT"
        loggerMayMap initial
        logger "COMPUTE FIXED POINT"
        res <- cont Set.empty (Map.map blankItem initial) (Map.keysSet initial)
        logger "FOUND FIXED POINT"
        loggerMap res
        logger "END FIXED POINT"
        logger ""
        return res
    where
        loggerLine k v = logger $ "    " ++ show k ++ " = " ++ show v
        loggerMap = mapM_ (uncurry loggerLine) . Map.toList
        loggerMayMap x | Map.size x > 5 = logger $ "    " ++ show (Map.size x) ++ " items"
                       | otherwise = loggerMap x
    
        def2 = blankItem def
        blankItem v = Item v Set.empty Set.empty
    
        cont listed x pending | Set.null pending = return $ Map.map value x
                              | otherwise = do
            -- calculate
            let k = next x pending
            Item{value=oldValue,dependsOn=oldDepends} <- return $ fromJust $ Map.lookup k x
            pending <- return $ Set.delete k pending
            (v,depends) <- execute (\k -> value $ Map.findWithDefault def2 k x) compute k
            v <- return $ combine oldValue v
            depends <- return $ Set.fromList depends

            -- add new items
            new <- return $ Set.filter (not . (`Map.member` x)) depends
            x <- return $ Map.union x (Map.fromAscList [(k, def2) | k <- Set.toAscList new])
            pending <- return $ Set.union pending new

            -- update the depends/requires, add new items
            delReq <- return $ Set.toList $ oldDepends `Set.difference` depends
            addReq <- return $ Set.toList $ depends `Set.difference` oldDepends
            x <- return $ apply x addReq (\y -> y{requiredBy = Set.insert k (requiredBy y)})
            x <- return $ apply x delReq (\y -> y{requiredBy = Set.delete k (requiredBy y)})
            
            -- update pending
            pending <- return $ if oldValue == v then pending
                                else Set.union pending (requiredBy $ fromJust $ Map.lookup k x)

            listed <- if listing v && not (k `Set.member` listed) then do
                          putStrLn $ "Partial: " ++ show k
                          return $ Set.insert k listed
                      else return listed

            -- add the new item to the map
            x <- return $ Map.adjust (\i -> i{dependsOn=depends, value=v}) k x
            loggerLine k v
            cont listed x pending


        apply mp ks f = foldl (\mp k -> Map.adjust f k mp) mp ks


-- calculate the next key, ideally optimal
-- pick the item which has the most requiredBy items already in the pending
-- since otherwise you'd be likely to add these anyway
-- HEURISTIC, requires experimentation
--
-- turns out bigger programs (HsColour) spend most of their time doing this
-- either find a way to only recompute the changed bit, or drop this to head
next :: Ord k => Map.Map k (Item k v) -> Set.Set k -> k
-- dead heuristic
next x pending | False = snd $ maximum [(f k, k) | k <- Set.toList pending]
    where f k = Set.size $ pending `Set.intersection` (requiredBy $ fromJust $ Map.lookup k x)

-- simple heuristic
next x pending = head $ Set.toList pending



-- do a one step execution
-- return the new value, and all the items used
execute
    :: (Eq v, Ord k, Show k)
    => (k -> v)                     -- query
    -> ((k -> IO v) -> k -> IO v)   -- compute
    -> k                            -- initial
    -> IO (v, [k])                  -- (result, dependencies)
execute query compute initial = do
        i <- newIORef []
        res <- compute (f i) initial
        depends <- readIORef i
        return (res, depends)
    where
        f i k = do
            modifyIORef i (k:)
            return $ query k
