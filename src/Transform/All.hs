
module Transform.All(transform) where

import Hite.Type
import Transform.Type
import Transform.Encode
import Transform.Driver
import General.General


transform :: Hite -> Hite
transform x = error $ output $ applyTransform $ encode x
