{- Point behaviors -}

module Point2B where

import qualified Point2 as P
import Fuzzy
import Behavior

-- Hugs bug (?) workaround.  See comment in VectorSpaceB.
-- Set binding strength to that of + and -.
-- infix 4 `pointPlusVector2`, `pointMinusVector2`, `pointMinusPoint2`

type Point2B = Behavior P.Point2

origin2           = lift0 P.origin2
point2XY          = lift2 P.point2XY (noI "point2XY")
point2Polar       = lift2 P.point2Polar (noI "point2Polar")
point2XYCoords    = lift1 P.point2XYCoords (noI "point2XYCoords")
point2PolarCoords = lift1 P.point2PolarCoords (noI "point2PolarCoords")
distance2         = lift2 P.distance2 (noI "distance2")
distance2Squared  = lift2 P.distance2Squared (noI "distance2Squared")
linearInterpolate2 = lift3 P.linearInterpolate2 (noI "linearInterpolate2")
(.+^)             = lift2 (P..+^) (noI "(.+^)")
(.-^)             = lift2 (P..-^) (noI "(.-^)")
(.-.)             = lift2 (P..-.) (noI "(.-.)")
