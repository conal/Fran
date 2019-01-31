-- 3D points (satic)
--
-- Last modified Sat Apr 26 23:07:35 1997
--
-- When Haskell has type relations (multi-parameter type classes), move most
-- of stuff to an AffineSpace type relation.
--

module Point3
        (
         Point3(..)
         ,origin3                -- :: Point3
         ,point3XYZ              -- :: RealVal -> RealVal -> Point3
--       ,point3Spherical        -- :: Radians -> Length  -> Point3
         ,point3XYZCoords        -- :: Point3  -> (RealVal, RealVal)
--       ,point3SphericalCoords  -- :: Point3  -> (Length, Radians)
         ,distance3              -- :: Point3 -> Point3 -> Length
         ,distance3Squared       -- :: Point3 -> Point3 -> Length
         ,linearInterpolate3     -- :: Point3 -> Point3 -> RealVal -> Point3

         ,(.+^)                  -- :: Point3 -> Vector3 -> Point3
         ,(.-^)                  -- :: Point3 -> Vector3 -> Point3
         ,(.-.)                  -- :: Point3 -> Point3  -> Vector3
        ) where

import Prelude
import BaseTypes
import VectorSpace
import Vector3
import Force

-- Set binding strength to that of + and -.
infix 4 .+^, .-^, .-.

data Point3 = Point3XYZ RealVal RealVal RealVal   deriving (Eq, Show)

origin3 :: Point3
origin3 = Point3XYZ 0 0 0

point3XYZ :: RealVal -> RealVal -> RealVal -> Point3
point3XYZ = Point3XYZ

point3XYZCoords :: Point3 -> (RealVal,RealVal,RealVal)
point3XYZCoords (Point3XYZ x y z) = (x,y,z)

{-
point3Spherical :: Length -> Radians -> Radians -> Point3
point3Spherical rho theta phi = ...

point3SphericalCoords :: Point3 -> (Length,Radians,Radians)
point3SphericalCoords (Point3XYZ x y z) = ...
-}

(.+^) :: Point3 -> Vector3 -> Point3
(Point3XYZ x y z) .+^ (Vector3XYZ dx dy dz) = Point3XYZ (x+dx) (y+dy) (z+dz)

(.-^) :: Point3 -> Vector3 -> Point3
(Point3XYZ x y z) .-^ (Vector3XYZ dx dy dz) = Point3XYZ (x-dx) (y-dy) (z-dz)

(.-.) :: Point3 -> Point3 -> Vector3
(Point3XYZ x y z) .-. (Point3XYZ x' y' z') = vector3XYZ (x-x') (y-y') (z-z')

distance3Squared :: Point3 -> Point3 -> Length
distance3Squared (Point3XYZ x y z) (Point3XYZ x' y' z') 
 = sq (x-x') + sq (y-y') + sq (z-z')
   where
    sq x = x*x

distance3 :: Point3 -> Point3 -> Length
distance3 p p' = sqrt (distance3Squared p p')

-- Linear interpolation
linearInterpolate3 :: Point3 -> Point3 -> RealVal -> Point3
linearInterpolate3 p p' frac =
  p .+^ (frac *^ (p' .-. p))

instance Forceable Point3 where
  force p@(Point3XYZ x y z) = force x `seq` force y `seq` force z `seq` p





