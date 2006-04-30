
import List

main = do x <- readFile "Preamble.core"
          writeFile "Preamble.hs.core" (strip x)



tupStart = ",CoreFunc (CoreApp (CoreVar \"Preamble.tup"
tupEnd = ")])"

strip x | tupStart `isPrefixOf` x = strip (dropTup x)
strip (x:xs) = x : strip xs
strip [] = []


dropTup x | tupEnd `isPrefixOf` x = drop (length tupEnd) x
dropTup (x:xs) = dropTup xs

        