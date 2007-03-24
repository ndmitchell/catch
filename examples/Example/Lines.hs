
module Lines where

main x = inlines x

inlines s = lines2 s id
  where
  lines2 []             acc = [acc []]
  lines2 (c:s)          acc = lines2 s (acc . (c:))

{-
ORIGINAL:

inlines s = lines' s id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id  -- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id  -- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id  -- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))


-}