
module General.Similar where


class Similar a where
    (~=) :: a -> a -> Bool
