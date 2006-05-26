
module Graph.CaseCheck(graphCaseCheck) where

import Graph.Type
import Graph.Create
import Graph.Solve
import IO
import Hite


graphCaseCheck :: String -> Handle -> Hite -> IO Bool
graphCaseCheck file hndl hite =
    do
        res <- mapM f $ createGraphs hite
        return $ and res
    where
        f (name,graph) = do putStrLn $ "Checking " ++ name
                            res <- solveGraph hite graph
                            putStrLn $ "Result: " ++ show res
                            return res
    
        graphs = createGraphs hite
