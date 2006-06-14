
-- Make sure there are no nested calls
-- Easy to do by introducing extra functions

module Hite.OneCall(cmd) where

import Hite.Type


cmd = cmdHitePure (const oneCall) "onecall"
            "Make sure there are no nested calls"


oneCall :: Hite -> Hite
oneCall x = x


