-- 3D Vectors (static)

module Vector3 
        (
         Vector3(..),
           -- instance Vector3 (Eq,Show,Num,VectorSpace,Transformable3)
         xVector3, yVector3, zVector3, -- :: Vector3  -- unit vectors

         vector3XYZ,             -- :: RealVal -> RealVal -> Vector3
         vector3XYZCoords,       -- :: Vector3 -> (RealVal, RealVal)
         vector3Spherical,          -- :: Length  -> Radians -> Vector3
         vector3SphericalCoords,    -- :: Vector3 -> (Length, Radians)
         -- instance  VectorSpace Vector3
        ) where

import BaseTypes
import VectorSpace
import Force

data Vector3 = Vector3XYZ RealVal RealVal RealVal   deriving (Eq,Show)

vector3XYZ :: RealVal -> RealVal -> RealVal -> Vector3
vector3XYZ = Vector3XYZ

xVector3,yVector3,zVector3 :: Vector3
xVector3 = Vector3XYZ 1 0 0
yVector3 = Vector3XYZ 0 1 0
zVector3 = Vector3XYZ 0 0 1

vector3XYZCoords :: Vector3 -> (RealVal,RealVal,RealVal)
vector3XYZCoords (Vector3XYZ x y z) = (x,y,z)

vector3Spherical :: Length -> Radians -> Radians -> Vector3
vector3Spherical rho theta phi =
  Vector3XYZ (rho * sinPhi * cosTheta)
             (rho * sinPhi * sinTheta)
             (rho * cosPhi)
 where
   cosTheta = cos theta; cosPhi = cos phi
   sinTheta = sin theta; sinPhi = sin phi 


vector3SphericalCoords :: Vector3 -> (Length,Radians,Radians)
vector3SphericalCoords v@(Vector3XYZ x y z) = (rho, theta, phi)
 where
   rho   = sqrt (x*x+y*y+z*z)
   theta = if x == 0 then 0 else atan (y/x)
   phi   = acos (z / rho)

instance  VectorSpace Vector3  where
  zeroVector = Vector3XYZ 0 0 0

  sc *^ (Vector3XYZ dx dy dz) = Vector3XYZ (sc*dx) (sc*dy) (sc*dz)

  (Vector3XYZ dx dy dz) ^+^ (Vector3XYZ dx' dy' dz') =
    Vector3XYZ (dx+dx') (dy+dy') (dz+dz')

  (Vector3XYZ dx dy dz) `dot` (Vector3XYZ dx' dy' dz')  =
     dx*dx' + dy*dy' + dz*dz'

instance  Num Vector3  where
  (+)    = (^+^)
  (*)	 = badOp "*"
  abs	 = badOp "abs"
  signum = badOp "signum"
  negate = negateVector
  -- (-) follows from negate and +
  fromInteger n = error ("fromInteger: cannot convert " ++ show n
                         ++ " to Vector3")

badOp opName = error ("bad operator on Vector3: " ++ opName)

instance Forceable Vector3 where
  force v@(Vector3XYZ x y z) = force x `seq` force y `seq` force z `seq` v

-- This is here so we can use + and - on Vector3B.
-- instance Ord Vector3
