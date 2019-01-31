--
-- RBMH   Lifted 3D point operations
--

module Point3B (
	Point3B, Point3,
	origin3,
	point3XYZ,    point3XYZCoords,
	--point3Polar, point3PolarCoords,
	distance3,   distance3Squared,
	(.+^#), (.-^#), (.-.#),
	linearInterpolate3,
	point3XYZCoords
	) where

import qualified Point3 as P
import Point3(Point3)
import Behavior
import BaseTypes
import Vector3B(Vector3B)

infix 4 .+^#, .-^#, .-.#

type Point3B = Behavior P.Point3

origin3             :: Point3B
point3XYZ            :: RealB -> RealB -> RealB -> Point3B
--point3Polar         :: RealB  -> RealB -> Point3B
point3XYZCoordsB      :: Point3B  -> Behavior (RealVal, RealVal, RealVal)
--point3PolarCoords   :: Point3B  -> Behavior (RealVal, RealVal, RealVal)
distance3           :: Point3B  -> Point3B  -> RealB
distance3Squared    :: Point3B  -> Point3B  -> RealB
linearInterpolate3  :: Point3B  -> Point3B  -> RealB -> Point3B
(.+^#)              :: Point3B  -> Vector3B -> Point3B
(.-^#)              :: Point3B  -> Vector3B -> Point3B
(.-.#)              :: Point3B	-> Point3B  -> Vector3B

origin3           = constantB P.origin3
point3XYZ          = lift3 P.point3XYZ
--point3Polar       = lift3 P.point3Polar
point3XYZCoordsB    = lift1 P.point3XYZCoords
--point3PolarCoords = lift1 P.point3PolarCoords
distance3         = lift2 P.distance3
distance3Squared  = lift2 P.distance3Squared
linearInterpolate3 = lift3 P.linearInterpolate3
(.+^#)             = lift2 (P..+^#)
(.-^#)             = lift2 (P..-^#)
(.-.#)             = lift2 (P..-.#)

point3XYZCoords :: Point3B -> (RealB, RealB, RealB)
point3XYZCoords = tripleBSplit . point3XYZCoordsB
