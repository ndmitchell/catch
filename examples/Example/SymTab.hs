
module SymTab where



main y = itiap y id


----
-- | Index Trees (storing indexes at nodes).

data IndTree = Fork IndTree
     deriving Show


itiap :: IndTree -> (IndTree -> b) -> b
itiap (Fork lt) k = itiap lt $ \lt2 -> k (Fork lt2)

