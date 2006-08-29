
module Hite.Transform(cmd) where

import Hite.Type
import Hite.Normalise
import Transform.All


cmd = cmdHitePure (const trans) "transform"
            "Do the new transformations"


trans :: Hite -> Hite
trans hite = transform $ normalise hite
