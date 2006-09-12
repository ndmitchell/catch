
-- override things in the Prelude

module Primitive where


foreign import primitive prim_ORD :: a -> b

foreign import primitive prim_EQ_W :: a -> b -> c
foreign import primitive prim_GT_W :: a -> b -> c
foreign import primitive prim_ADD_W :: a -> b -> c
foreign import primitive prim_LE_W :: a -> b -> c
foreign import primitive prim_LT_W :: a -> b -> c
foreign import primitive prim_SUB_W :: a -> b -> c
foreign import primitive prim_GE_W :: a -> b -> c
foreign import primitive prim_NE_W :: a -> b -> c
foreign import primitive prim_MUL_W :: a -> b -> c
foreign import primitive prim_NEG_W :: a -> b -> c
foreign import primitive prim_REM :: a -> b -> c
foreign import primitive prim_QUOT :: a -> b -> c
foreign import primitive prim_SEQ :: a -> b -> c



data Char = Char
data Int = Int
data Integer = Integer
data Float = Float
data Double = Double
