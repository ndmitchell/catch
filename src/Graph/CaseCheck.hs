
module Graph.CaseCheck(graphCaseCheck) where

import Graph.Type
import Graph.Create
import Graph.Solve
import IO
import Hite


graphCaseCheck :: String -> Handle -> Hite -> IO Bool
graphCaseCheck file hndl hite =
    do
        let graph = createGraph hite
        putStrLn "Checking graph"
        res <- solveGraph file hite graph
        putStrLn $ "Result: " ++ show res
        return res
