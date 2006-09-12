
{-! global: GhcBinary !-}

module Hite.TypeType where

data TyType = TyCon String [TyType]
            | TyFree String
            | TyNone
            deriving (Eq, Show, Read)

