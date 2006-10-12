
{-! global: GhcBinary !-}

module Hite.TypeType where

data TyType = TyCon String [TyType]
            | TyFree String
            | TyNone
            deriving Eq


instance Show TyType where
    show x = f False x
        where
            f b (TyFree x) = x
            f b (TyCon n xs) = ['('|b] ++ n ++ concatMap ((' ':) . f True) xs ++ [')'|b]

