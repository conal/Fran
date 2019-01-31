-- 2D transforms.
-- 
-- Last modified Mon Sep 16 14:54:42 1996
module Transform2 
        (
         Transform2(..),
           {- instance {Eq,  -- currently structural
                        Ord, -- currently empty
                        Text}
                       Transform2
           -}
         Transformable2((*%)),
         Translateable2(translate2),
         Scaleable2(scale2),

         transform3x2,          -- :: RealVal -> RealVal -> RealVal -> RealVal -> RealVal -> RealVal -> Transform2
         identity2,             -- :: Transform2
         rotate2,               -- :: Radians -> Transform2
         compose2,              -- :: Transform2 -> Transform2 -> Transform2
         uscale2,               -- :: RealVal -> Transform2
         inverse2,              -- :: Transform2 -> Transform2

          {- Not implemented yet 
         shear2,                -- :: RealVal -> Transform2
         isSingular,            -- :: Transform2 -> Bool
           -}
        ) where

import BaseTypes
import Vector2
import Point2

infixr 7 *%,`compose2`           -- transform apply and compose

data Transform2 =
  X RealVal RealVal RealVal RealVal RealVal RealVal
  deriving (Eq,Text)

-- Renamed constructor
transform3x2 = X


class Translateable2 a where
 translate2 :: a -> Transform2

class Scaleable2 a where
 scale2 :: a -> Transform2

class Transformable2 a where
  (*%)  ::  Transform2 -> a -> a

-- Overloadings of translate

instance Translateable2 Vector2 where
  translate2 (Vector2XY dx dy) =  X   1    0  dx
				      0    1  dy


-- Should we even have this one?
instance Translateable2 Point2 where
  translate2 (Point2XY x y) = translate2 (vector2XY x y)

-- Worth having?  (note: Double == RealVal)
instance Translateable2 Double where
  translate2 r = translate2 (vector2XY r r)
  
{- We'd really like to be able to do this but cannot
instance Translateable2 (RealVal,RealVal) where
  translate (x,y) = translate (vector2XY x y)
-}

{- Overloadings of scale -}
instance Scaleable2 Vector2 where
  scale2 (Vector2XY sx sy) = X  sx    0  0   
				0    sy  0

  
instance Scaleable2 Double where
  scale2 f = scale2 (Vector2XY f f)
  
-- There is a problem with the overloading for Double: given an expression
-- like "scale2 5.0", there isn't enough information to resolve the
-- overloading of 5.0, so you have to say "scale2 (5.0::Double)" instead,
-- which is a pain.  Thus the convenience function "uscale" (uniform
-- scale) below.


identity2 = X  1   0   0
	       0   1   0

rotate2 a = X   c (-s)  0   
		s    c  0  
 where 
  c = cos a
  s = sin a

(X a b c d e f) `compose2` (X a' b' c' d' e' f') =
   X (a*a'+b*d') (a*b'+b*e') (a*c'+b*f'+c)
     (d*a'+e*d') (d*b'+e*e') (d*c'+e*f'+f)


-- Uniform scale.  See comment above about scale2 on Double

uscale2 :: RealVal -> Transform2
uscale2 = scale2


inverse2 :: Transform2 -> Transform2

inverse2 xf@(X a b c d e f) =
 X ( e/det) (-b/det) ((-c*e + b*f)/det)
   (-d/det) ( a/det) (( c*d - a*f)/det)
 where
   det = -(b*d) + a*e

-- We could educate Compose2 about transformation identities, such as
-- "Compose2 (Rotate2 theta) (Rotate2 rho) == Rotate2D (theta+rho)".
-- Also, throw in linears (2x2 matrices) and affines (2x3).  Probably
-- better to put these smarts into the lifted versions instead.  Better
-- yet, have the compiler be smart enough to lift the smarts.  Reduce to a
-- single constructor that does rotate/scale/translate.  Probably postpone
-- rotate until DirectDraw supports it.

instance Transformable2 Point2 where
 (X a b c d e f) *% (Point2XY x y) =
   point2XY (a*x+b*y+c) (d*x+e*y+f)


-- Vector transformation is defined in terms of point transformation.
-- BTW, translations have no effect on vectors.

instance Transformable2 Vector2 where
 xf *% (Vector2XY x y) =
  xf *% (point2XY x y) `pointMinusPoint2` xf *% origin2
