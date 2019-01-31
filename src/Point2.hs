-- RBMH 2d point abstraction.
--
-- Last modified Sat Sep 07 23:23:10 1996
--
-- When Haskell has type relations (multi-parameter type classes), move most
-- of stuff to an AffineSpace type relation.
--
-- To do: replace names like "pointPlusVector2" with arithmetic-looking
-- operators.


module Point2
        (
         Point2(..),
         origin2,                -- :: Point2
         point2XY,               -- :: RealVal    -> RealVal    -> Point2
         point2Polar,            -- :: Radians -> Length    -> Point2
         point2XYCoords,         -- :: Point2  -> (RealVal, RealVal)
         point2PolarCoords,      -- :: Point2  -> (Length, Radians)
         distance2,              -- :: Point2 -> Point2 -> Length
         distance2Squared,       -- :: Point2 -> Point2 -> Length
         lerp2,                  -- :: Point2 -> Point2 -> RealVal -> Point2

         pointPlusVector2,       -- :: Point2 -> Vector2 -> Point2
         pointMinusVector2,      -- :: Point2 -> Vector2 -> Point2
         pointMinusPoint2        -- :: Point2 -> Point2  -> Vector2
        ) where

import Prelude
import BaseTypes
import VectorSpace
import Vector2

-- Set binding strength to that of + and -.
infix 4 `pointPlusVector2`, `pointMinusVector2`, `pointMinusPoint2`

data Point2 = Point2 RealVal RealVal   deriving (Eq, Text)

origin2 :: Point2
origin2 = Point2 0 0

point2XY :: RealVal -> RealVal -> Point2
point2XY = Point2

point2Polar :: Length -> Radians -> Point2
point2Polar r t = Point2 (r * cos t) (r * sin t)

point2XYCoords :: Point2 -> (RealVal,RealVal)
point2XYCoords (Point2 x y) = (x,y)

point2PolarCoords :: Point2 -> (Radians,Length)
point2PolarCoords (Point2 x y) = (magnitude (Vector2 x y), atan2 y x)

pointPlusVector2 :: Point2 -> Vector2 -> Point2
pointPlusVector2  (Point2 x y) (Vector2 dx dy) = Point2 (x+dx) (y+dy)

pointMinusVector2 :: Point2 -> Vector2 -> Point2
pointMinusVector2 (Point2 x y) (Vector2 dx dy) = Point2 (x-dx) (y-dy)

pointMinusPoint2 :: Point2 -> Point2 -> Vector2
pointMinusPoint2 (Point2 x1 y1) (Point2 x2 y2) = vector2XY (x1-x2) (y1-y2)

distance2Squared :: Point2 -> Point2 -> Length
distance2Squared (Point2 x1 y1) (Point2 x2 y2) 
 = sq (x1-x2) + sq (y1-y2)
   where
    sq x = x*x

distance2 :: Point2 -> Point2 -> Length
distance2 a b = sqrt (distance2Squared a b)

-- Linear interpolation
lerp2 :: Point2 -> Point2 -> RealVal -> Point2
lerp2 p1 p2 frac = p1 `pointPlusVector2`
                     (frac `scaleVector` (p2 `pointMinusPoint2` p1))
