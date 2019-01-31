-- 3D transforms.
-- 
-- Last modified Thu Oct 02 16:37:28 1997
module Transform3 
        (
          Transform3(..)
         ,Transformable3((**%))
         ,Translateable3(translate3)
         ,Scaleable3(scale3)

         ,identity3             -- :: Transform3
         ,rotate3               -- :: Radians -> Transform3
         ,uscale3               -- :: RealVal -> Transform3
         ,compose3              -- :: Transform3 -> Transform3 -> Transform3
      -- ,inverse3              -- :: Transform3 -> Transform3
         ,hFrameSetTransform    -- :: HFrame -> Transform3 -> IO ()

        ) where

import BaseTypes
import Vector3
import Point3

import qualified HSpriteLib as SL

infixr 7 **%,`compose3`         -- transform apply and compose


data Transform3 =
    Identity3
  | Translate3 Vector3
  | Rotate3    Vector3 RealVal    -- axis and angle
  | Scale3     Vector3
  | Compose3   Transform3 Transform3  -- outer and inner
 deriving (Eq,Show)

identity3 = Identity3
rotate3 = Rotate3
compose3 = Compose3


class Translateable3 a where
 translate3 :: a -> Transform3

class Scaleable3 a where
 scale3 :: a -> Transform3

class Transformable3 a where
  (**%)  ::  Transform3 -> a -> a


instance Translateable3 Vector3 where
  translate3 = Translate3


-- Should we even have this one?
instance Translateable3 Point3 where
  translate3 p = translate3 (p .-.# origin3)

 
{-  We'd really like to be able to do this but cannot
instance Translateable3 (RealVal,RealVal,RealVal) where
  translate (x,y,z) = translate (vector3XYZ x y z)
-}


instance Scaleable3 Vector3 where
  scale3 = Scale3
  
instance Scaleable3 Double where
  scale3 f = scale3 (Vector3XYZ f f f)
  
-- There is a problem with the overloading for Double: given an expression
-- like "scale3 5.0", there isn't enough information to resolve the
-- overloading of 5.0, so you have to say "scale3 (5.0::Double)" instead,
-- which is a pain.  Thus the convenience function "uscale" (uniform
-- scale) below.

-- Uniform scale.

uscale3 :: RealVal -> Transform3
uscale3 = scale3


-- For interacting with D3DRM

hFrameSetTransform :: SL.HFrame -> Transform3 -> IO ()

hFrameSetTransform frame xf =
  do SL.hFrameClearTransform frame
     transRec xf
 where
   transRec Identity3 = return ()

   transRec (Translate3 (Vector3XYZ dx dy dz)) =
     SL.hFrameTranslate frame dx dy dz

   transRec (Rotate3 (Vector3XYZ x y z) theta) =
     SL.hFrameRotate frame x y z theta

   transRec (Scale3 (Vector3XYZ x y z)) =
     SL.hFrameScale frame x y z

   transRec (Compose3 outer inner) =
     do transRec inner
        transRec outer
