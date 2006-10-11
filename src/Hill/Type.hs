
{-! global: GhcBinary !-}

module Hill.Type(module Hill.Type, module Hite.DataType, module Hite.TypeType) where

import Hite.DataType
import Hite.TypeType


data Hill = Hill {datas :: [Data], funcs :: [Func]}

instance QDatas Hill where
    rawDatas = datas


data Func = Func FuncName [Int] Expr

data Expr = 
            -- atoms
            Var Int
          | Fun FuncName
          | Const Const
            
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


data Const = AInt Int
           | AInteger Integer
           | AFloat Float
           | ADouble Double
           | AChar Char
           | AString String
           | ACtor CtorName


data Alt = Default Expr
         | Alt Const Expr
