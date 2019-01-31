-- Picking Image values
--
-- Last modified Tue Nov 05 14:50:52 1996

module PickImage where

import BaseTypes
import Point2
import Vector2
import Transform2
import Image

-- Very simple for now.
pick2 :: Image -> Point2 -> Bool
pick2 im pt = pick' pt False im
 where
  pick' p@(Point2XY x y) fill im =
    case im of
      EmptyImage -> False
      Circle -> (distance2Squared p origin2 <= 1)
      Square -> abs x <= oneOverSqrtTwo && abs y <= oneOverSqrtTwo
      Bitmap (Vector2XY dx dy) _ -> abs x <= dx && abs y <= dy
      Line _ _        -> False
      Polyline _      -> False
      Polygon _       -> False
      Bezier _ _ _ _  -> False
      RenderedText _  -> False
      WithColor _ im  -> pick' p True im
      Over im1 im2 ->
        pick' p fill im1 ||
        pick' p fill im2
      TransformedImage xf im ->
        pick' (inverse2 xf *% p) fill im
      BBoxed2 p1 p2 im ->
        insideRect p1 p2 p &&
        pick' p fill im

oneOverSqrtTwo = 1 / sqrt 2  :: RealVal

--
-- Support picking functions.
--

-- Within interval, endpoints not ordered
within :: RealVal -> RealVal -> RealVal -> Bool
within a1 a2 v = (v - a1) * (v - a2) <= 0 

-- Or, equivalently: a1 <= v && v <= a2 || a2 <= v && v <= a1.  Thanks to
-- Gary Shu Ling for -/* version above.

insideRect :: Point2 -> Point2 -> Point2 -> Bool
insideRect (Point2XY x1 y1) (Point2XY x2 y2) (Point2XY a b) = 
 within x1 x2 a &&
 within y1 y2 b
