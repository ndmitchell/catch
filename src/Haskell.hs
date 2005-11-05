

module Haskell(module Haskell.Syntax, Haskell) where


import Haskell.Syntax
import Haskell.Parser

type Haskell = HsModule

instance Read HsModule where
    readsPrec n x = case parseModule x of
                        ParseOk x -> [(x,"")]
                        _ -> error "Failed to parse the Haskell"
