
module Transform.All(transform, transform2, transform3) where

import Hite.Type
import Transform.Type
import Transform.Encode
import Transform.Decode
import Transform.Driver
import Transform.Reform
import Transform.Specialise
import Transform.EncodeSpec
import General.General


transform :: Hite -> Hite
transform x = decode $ applyTransform $ encode x

transform2 :: Hite -> Hite
transform2 x = decode $ reform $ encode x


transform3 :: Hite -> Hite
transform3 x = decode $ specialise $ encodeSpec $ encode x
