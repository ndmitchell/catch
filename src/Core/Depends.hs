
module Core.Depends(depends) where

import Core.Type
import List


depends :: Core -> [String]
depends core = nub [reverse $ tail $ dropWhile (/= '.') $ reverse x | CoreVar x <- allCore core, '.' `elem` x]
