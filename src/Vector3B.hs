-- 3D Vector behaviors
--
-- Last modified Tue Oct 21 14:15:39 1997

module Vector3B (
	Vector3B, Vector3,
	xVector3, yVector3, zVector3,
	vector3XYZ, vector3XYZCoords,
	vector3Spherical,
        vector3SphericalCoords
	) where

import qualified Vector3 as V
import Vector3(Vector3)
import Behavior
import BaseTypes

type Vector3B = Behavior V.Vector3

xVector3, yVector3 :: Vector3B   -- unit vectors
vector3XYZ         :: RealB -> RealB -> RealB -> Vector3B
vector3Spherical   :: Behavior Length -> Behavior Radians
                   -> Behavior Radians -> Vector3B
vector3XYZCoords   :: Vector3B -> Behavior (RealVal, RealVal, RealVal)
vector3SphericalCoords :: Vector3B -> Behavior (Length, Radians, Radians)

xVector3           = constantB V.xVector3
yVector3           = constantB V.yVector3
zVector3           = constantB V.zVector3
vector3XYZ         = lift3 V.vector3XYZ
vector3Spherical   = lift3 V.vector3Spherical
vector3XYZCoords   = lift1 V.vector3XYZCoords
vector3SphericalCoords = lift1 V.vector3SphericalCoords

-- Vector space operations lifted in VectorSpaceB.hs
