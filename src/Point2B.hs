{- Point behaviors -}

module Point2B where

import qualified Point2 as P
import Behavior

-- Hugs bug (?) workaround.  See comment in VectorSpaceB.
-- Set binding strength to that of + and -.
-- infix 4 `pointPlusVector2`, `pointMinusVector2`, `pointMinusPoint2`

type Point2B = Behavior P.Point2

origin2           = lift0 P.origin2
point2XY          = lift2 P.point2XY
point2Polar       = lift2 P.point2Polar
point2XYCoords    = lift1 P.point2XYCoords
point2PolarCoords = lift1 P.point2PolarCoords
distance2         = lift2 P.distance2
distance2Squared  = lift2 P.distance2Squared
linearInterpolate2 = lift3 P.linearInterpolate2
pointPlusVector2  = lift2 P.pointPlusVector2
pointMinusVector2 = lift2 P.pointMinusVector2
pointMinusPoint2  = lift2 P.pointMinusPoint2





