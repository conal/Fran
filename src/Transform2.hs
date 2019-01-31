-- 2D transforms.
-- 
-- Last modified Wed May 06 22:01:18 1998
module Transform2 
        (
           Transform2(..)
         , Transformable2((*%))
         , translateUscaleRotate2-- ::  Vector2 -> RealVal -> RealVal -> Transform2
         , identity2             -- :: Transform2
         , translate2            -- :: Vector2 -> Transform2
         , rotate2               -- :: Radians -> Transform2
         , compose2              -- :: Transform2 -> Transform2 -> Transform2
         , uscale2               -- :: RealVal -> Transform2
         , inverse2              -- :: Transform2 -> Transform2
         , factorTransform2      -- :: Transform2 -> (Vector2, RealVal, RealVal)

        ) where

import BaseTypes
import Vector2
import Point2
import VectorSpace

infixr 7 *%,`compose2`           -- transform apply and compose


-- Form is translate of scale of rotate, with the rotation angle in
-- radians.
data Transform2 = Transform2 Vector2 RealVal Radians deriving Show

translateUscaleRotate2 ::  Vector2 -> RealVal -> Radians -> Transform2
translateUscaleRotate2 = Transform2

factorTransform2 :: Transform2 -> (Vector2, RealVal, RealVal)
factorTransform2 (Transform2 mot sc rot) = (mot,sc,rot)

identity2 :: Transform2
identity2 = Transform2 zeroVector 1  0

rotate2 :: RealVal -> Transform2
rotate2 theta = Transform2 zeroVector 1 theta

-- For now just uniform scale, sorry.  The reason is that it's harder to
-- do the normalization with non-uniform scale.
uscale2 :: RealVal -> Transform2
uscale2 sc = Transform2 zeroVector sc 0

translate2 :: Vector2 -> Transform2
translate2 v =  Transform2 v 1 0


compose2 :: Transform2 -> Transform2 -> Transform2
xfO@(Transform2 motO scO rotO) `compose2` Transform2 motI scI rotI =
  Transform2 mot sc rot
 where
   -- Move motI left through rotO and scO and add motO.  (Recall that
   -- translation has no effect on vectors.)
   mot = motO + xfO *% motI
   -- Then move scI left past rotO.  Uniform scale commutes with rotation.
   sc  = scO  * scI
   rot = rotO + rotI

inverse2 :: Transform2 -> Transform2
inverse2 (Transform2 mot sc rot) =
            rotate2    (-rot)
 `compose2` uscale2    (1/sc)
 `compose2` translate2 (- mot)
  

class Transformable2 a where
  (*%)  ::  Transform2 -> a -> a

instance (Transformable2 a, Transformable2 b) => Transformable2 (a,b) where
  xf *% (a,b) = (xf *% a, xf *% b)

instance (Transformable2 a, Transformable2 b, Transformable2 c)
  => Transformable2 (a,b,c) where
  xf *% (a,b,c) = (xf *% a, xf *% b, xf *% c)

instance Transformable2 Point2 where
 Transform2 (Vector2XY dx dy) scale angle *% Point2XY x y =
   Point2XY (dx + scale * (x * c - y * s))
            (dy + scale * (x * s + y * c))
   where
     c = cos angle
     s = sin angle

-- Vector transformation is defined as customary, by applying just the
-- linear portion (no translation).
instance Transformable2 Vector2 where
 Transform2 _ scale angle *% Vector2XY x y =
   Vector2XY (scale * (x * c - y * s))
             (scale * (x * s + y * c))
  where
    c = cos angle
    s = sin angle

