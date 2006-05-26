
module Graph.Solve(solveGraph) where

import Graph.Type
import Graph.Draw
import Hite


solveGraph :: Hite -> Graph -> IO Bool
solveGraph hite graph = do drawGraph graph "Temp-Graph"
                           return False


{-
-- Remove all control->control nodes
controlReduction :: Graph -> Graph
controlReduction (Graph nodes) = gc $ Graph $ map f nodes
    where
        f (Node edges (Just r)) = undefined

-}
