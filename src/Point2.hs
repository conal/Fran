-- RBMH 2d point abstraction.
--
-- Last modified Wed Nov 05 16:24:57 1997
--
-- When Haskell has type relations (multi-parameter type classes), move most
-- of stuff to an AffineSpace type relation.
--
-- To do: replace names like "pointPlusVector2" with arithmetic-looking
-- operators.


module Point2
        (
         Point2(..)
         ,origin2                -- :: Point2
         ,point2XY               -- :: RealVal    -> RealVal    -> Point2
         ,point2Polar            -- :: Radians -> Length    -> Point2
         ,point2XYCoords         -- :: Point2  -> (RealVal, RealVal)
         ,point2PolarCoords      -- :: Point2  -> (Length, Radians)
         ,distance2              -- :: Point2 -> Point2 -> Length
         ,distance2Squared       -- :: Point2 -> Point2 -> Length
         ,linearInterpolate2     -- :: Point2 -> Point2 -> RealVal -> Point2
	 ,swapCoordSys4Pt	 -- :: Point2 -> Point2 -> Point2; GSL
	 ,swapCoordSysInv4Pt	 -- :: Point2 -> Point2 -> Point2; GSL

	 ,p2v			 -- :: Point2 -> Vector2
	 ,v2p			 -- :: Vector2 -> Point2; GSL

	 ,mirrorX		 -- :: Point2 -> Point2
	 ,mirrorY		 -- :: Point2 -> Point2
	 ,mirrorXY		 -- :: Point2 -> Point2

         ,(.+^)                  -- :: Point2 -> Vector2 -> Point2
         ,(.-^)                  -- :: Point2 -> Vector2 -> Point2
         ,(.-.)                  -- :: Point2 -> Point2  -> Vector2
        ) where

import Prelude
import BaseTypes
import VectorSpace
import Vector2
import Force

-- Set binding strength to that of + and -.
infix 4 .+^, .-^, .-.

data Point2 = Point2XY RealVal RealVal   deriving (Eq, Show, Read)

origin2 :: Point2
origin2 = Point2XY 0 0

point2XY :: RealVal -> RealVal -> Point2
point2XY = Point2XY

point2Polar :: Length -> Radians -> Point2
point2Polar r t = Point2XY (r * cos t) (r * sin t)

point2XYCoords :: Point2 -> (RealVal,RealVal)
point2XYCoords (Point2XY x y) = (x,y)

point2PolarCoords :: Point2 -> (Radians,Length)
point2PolarCoords (Point2XY x y) =
  (magnitude (Vector2XY x y),
   if (x==0 && y==0) then 0 else atan2 y x)

(.+^) :: Point2 -> Vector2 -> Point2
(Point2XY x y) .+^ (Vector2XY dx dy) = Point2XY (x+dx) (y+dy)

(.-^) :: Point2 -> Vector2 -> Point2
(Point2XY x y) .-^ (Vector2XY dx dy) = Point2XY (x-dx) (y-dy)

(.-.) :: Point2 -> Point2 -> Vector2
(Point2XY x1 y1) .-. (Point2XY x2 y2) = vector2XY (x1-x2) (y1-y2)

distance2Squared :: Point2 -> Point2 -> Length
distance2Squared (Point2XY x1 y1) (Point2XY x2 y2) 
 = sq (x1-x2) + sq (y1-y2)
   where
    sq x = x*x

distance2 :: Point2 -> Point2 -> Length
distance2 a b = sqrt (distance2Squared a b)

-- Linear interpolation
linearInterpolate2 :: Point2 -> Point2 -> RealVal -> Point2
linearInterpolate2 p1 p2 frac =
  p1 .+^ (frac *^ (p2 .-. p1))

instance Forceable Point2 where
  force p@(Point2XY x y) = force x `seq` force y `seq` p

-----------------------------------------------------------------
-- swap coordinate system between that origin in center of are
-- and that origin in upper-left corner; first argument is for
-- new origin; both points are in Fran coordinate system
-----------------------------------------------------------------

swapCoordSys4Pt :: Point2 -> Point2 -> Point2
swapCoordSys4Pt newOrigin pt =
  let Vector2XY x y = pt .-. newOrigin
  in  Point2XY x (-y)

swapCoordSysInv4Pt :: Point2 -> Point2 -> Point2
swapCoordSysInv4Pt oldOrigin (Point2XY x y) =
  oldOrigin .+^ Vector2XY x (-y)

-----------------------------------------------------------------
-- convert between Point2 and Vector2
-----------------------------------------------------------------

p2v :: Point2 -> Vector2
p2v p = p .-. origin2

v2p :: Vector2 -> Point2
v2p v = origin2 .+^ v

-----------------------------------------------------------------
-- mirroring
-----------------------------------------------------------------

mirrorX :: Point2 -> Point2
mirrorX (Point2XY x y) = Point2XY x (-y)

mirrorY :: Point2 -> Point2
mirrorY (Point2XY x y) = Point2XY (-x) y

mirrorXY :: Point2 -> Point2
mirrorXY = mirrorX . mirrorY