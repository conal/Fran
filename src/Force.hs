
-- The Forceable class of objects that can be forced.

module Force where

class Forceable a where
    force :: a -> a


{-class (Forceable a) => ForceableIdentity a where
--    force :: a -> a
    force = id

instance ForceableIdentity Int
instance ForceableIdentity Float
instance ForceableIdentity Double
-}

instance Forceable Int where
  force = id

instance Forceable Double where
  force = id

instance Forceable Float where
  force = id

instance Forceable () where
  force = id

instance (Forceable a, Forceable b) => Forceable (a, b) where
  force (a, b) = (force a, force b)

instance (Forceable a) => Forceable [a] where
  force [] = []
  force xs@(x:xs') = force x `seq` force xs' `seq` xs
