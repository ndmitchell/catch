
module Graph.Type where

import Data.List


data Graph = Graph [Node]
data Node = Node {nodeName :: String, edges :: [Int], rewrite :: Maybe Rewrite}

data Rewrite = Rewrite GExp GExp
             | GraphEnd

data GExp = GVar String
          | GCtor String [GExp]
          | GFunc String GExp
          | GStr String



-- Which nodes are reachable from an initial node
reachable :: Graph -> Int -> [Int]
reachable (Graph nodes) x = fixSet f [x]
    where
        f x = edges (nodes !! x)



-- perform a garbage collection
-- useful to stop deletion of nodes being required ever
gc :: Graph -> Graph
gc x = x



-- find the fixed point of a set
fixSet :: Eq a => (a -> [a]) -> [a] -> [a]
fixSet f elems = fix2 f elems []
    where
        fix2 f [] _ = []
        fix2 f x done = x ++ fix2 f (x2 \\ done2) done2
            where
                done2 = x ++ done
                x2 = nub $ concatMap f x
