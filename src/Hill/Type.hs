
{-! global: GhcBinary !-}

module Hill.Type where

import Hite.DataType
import Hite.TypeType


data Hill = Hill {datas :: [Data], funcs :: [Func]}

data Func = Func FuncName [Int] Expr



data Expr = 
            -- atoms
            Var Int
          | Fun FuncName
          
            -- constants
          | Str String
          | Num Integer
          | Chr Char
          | Flt Double
            
            -- calls
          | Call FuncName [Expr]
          | Make CtorName [Expr]
          | Prim String   [Expr]
          
            -- structure
          | Sel Expr CtorArg
          | Let [(Int, Expr)] Expr
          | Case Expr [Alt]
          -- | MCase ...
          
            -- higher order bits
          | Lambda Int Expr
          | Apply Expr [Expr]
          
            -- other stuff
          | Error Expr



data Alt = Default
         | Alt CtorName Expr
