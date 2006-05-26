
module Graph.Solve(solveGraph) where

import Graph.Type
import Hite


solveGraph :: Hite -> Graph -> IO Bool
solveGraph hite graph = return False


-- Remove all control->control nodes
controlReduction :: Graph -> Graph
controlReduction (Graph nodes) = gc $ Graph $ map f nodes
    where
        f (Node edges (Just r)) = undefined

