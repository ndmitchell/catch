
module Transform.All(transform, transform2) where

import Hite.Type
import Transform.Type
import Transform.Encode
import Transform.Decode
import Transform.Driver
import Transform.Reform
import General.General


transform :: Hite -> Hite
transform x = decode $ applyTransform $ encode x

transform2 :: Hite -> Hite
transform2 x = decode $ reform $ encode x
