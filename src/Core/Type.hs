
module Core.Type where


data Core = Core [CoreFunc]
            deriving (Show, Read)


data CoreFunc = CoreFunc CoreExpr CoreExpr
                deriving (Show, Read)


data CoreExpr = CoreCon String
              | CoreVar String
              | CoreApp CoreExpr [CoreExpr]
              | CoreInt Int
              | CoreChr Char
              | CoreStr String
              | CoreCase CoreExpr [(CoreExpr,CoreExpr)]
              | CoreLet [CoreFunc] CoreExpr
              | CorePos String CoreExpr
                deriving (Show, Read)
