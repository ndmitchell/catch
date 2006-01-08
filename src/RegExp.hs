
{- |
    General module used as a testbed, imports and exports things.
    Most end user programs probably just want to import this module.
-}

module RegExp(
    -- | The data definitions
    module RegExp.Type,
    
    -- | Implementations of 'readBy' and 'showBy'
    module RegExp.Parse,
    
    -- | Various properties on regular expressions
    module RegExp.Prop,
    module RegExp.Enum,
    module RegExp.Blur
    )
    where

import RegExp.Type
import RegExp.Parse
import RegExp.Prop
import RegExp.Enum
import RegExp.Blur


import General.Similar


instance Eq x => Eq (RegExp x) where
    a == b = a ~= b
