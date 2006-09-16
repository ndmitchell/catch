
module Hite.Transform(cmd1,cmd2,cmd3) where

import Hite.Type
import Hite.Normalise
import Transform.All


cmd1 = cmdHitePure (const trans) "transform"
            "Do the new transformations"

cmd2 = cmdHitePure (const trans2) "transform2"
            "Do the new transformations (2)"

cmd3 = cmdHitePure (const trans3) "transform3"
            "Do the new transformations (3)"


trans :: Hite -> Hite
trans hite = transform $ normalise hite


trans2 :: Hite -> Hite
trans2 hite = transform2 $ normalise hite


trans3 :: Hite -> Hite
trans3 hite = transform3 $ normalise hite


