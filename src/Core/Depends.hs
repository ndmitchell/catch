
module Core.Depends(depends) where

import Core.Type
import List
import General.TextUtil
import Char


depends :: Core -> [String]
depends core = nub [head items | CoreVar x <- allCore core,
                                 let items = splitList x ".",
                                 length items > 1,
                                 isUpper $ head $ last items
                                 ]
