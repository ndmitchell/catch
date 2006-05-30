
module Graph.Type where

import Data.List
import Data.Maybe


type Graph = [Node]
data Node = Node {nodeName :: String, edges :: [Int], rewrite :: [Rewrite]}

data Rewrite = Rewrite GExp GExp
             | GraphEnd

data GExp = GVar String
          | GCtor String [GExp]
          | GFunc String GExp
          | GStr String


isGVar (GVar _) = True; isGVar _ = False

isGraphEnd (GraphEnd) = True; isGraphEnd _ = False


mapGExp :: (GExp -> GExp) -> GExp -> GExp
mapGExp f x = f $ case x of
                  GCtor a b -> GCtor a (map (mapGExp f) b)
                  GFunc a b -> GFunc a (mapGExp f b)
                  x -> x



-- Which nodes are reachable from an initial node
gReachable :: Graph -> Int -> [Int]
gReachable nodes x = fixSet f [x]
    where
        f x = edges (nodes !! x)



-- perform a garbage collection
-- useful to stop deletion of nodes being required ever
gc :: Graph -> Graph
gc graph = map f [graph !! n | n <- new]
    where
        new = sort $ gReachable graph 0
        ren = zip new [0..]
        
        f node = node{edges = map (\x -> fromJust $ lookup x ren) (edges node)}




-- find the fixed point of a set
fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _ = []
        fix2 f x done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x
