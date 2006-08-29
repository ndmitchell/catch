
module Checker.Statistics(statistics) where

import Hite
import General.General


statistics :: Hite -> IO ()
statistics hite@(Hite dat func) = do
        putStrLn $ "Functions        " ++ show (length func)
        putStrLn $ "Case expressions " ++ show lcases
        putStrLn $ "   incomplete    " ++ show (lcases-complete)
        putStrLn $ "   complete      " ++ show complete
    where
        cases = [x | x@(Case _ _) <- allExpr hite]
        lcases = length cases
        complete = length $ filter isComplete cases
        
        
        isComplete (Case _ []) = False
        isComplete (Case _ alts@((one,_):_)) = cs `setEq` (map fst alts)
            where cs = map ctorName $ ctors $ getCtor hite one
