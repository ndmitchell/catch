
module Typey.Reason(Reason(..), reasonSubtype) where

import Hite
import Typey.Subtype


data Reason = ReasonArgs [(FuncArg, TSubtype)] Reason
            | ReasonUnion TSubtype [Reason]
            | ReasonLookup TSubtype String
            | ReasonFollow TSubtype String Reason
            | ReasonApply TSubtype Reason Reason



reasonSubtype :: Reason -> TSubtype
reasonSubtype (ReasonArgs _ x) = reasonSubtype x
reasonSubtype (ReasonUnion x _) = x
reasonSubtype (ReasonLookup x _) = x
reasonSubtype (ReasonFollow x _ _) = x
reasonSubtype (ReasonApply x _ _) = x

