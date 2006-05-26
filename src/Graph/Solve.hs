
module Graph.Solve(solveGraph) where

import Graph.Type


solveGraph :: Graph -> Graph
solveGraph x = x


-- Remove all control->control nodes
controlReduction :: Graph -> Graph
controlReduction (Graph nodes) = gc $ Graph $ map f nodes
    where
        f (Node edges (Just r)) = undefined

