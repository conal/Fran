{- Point behaviors -}

module Point2B where

import qualified Point2 as P
import Behavior

-- Set binding strength to that of + and -.
infix 4 .+^, .-^, .-.

type Point2B = Behavior P.Point2

origin2            = constantB P.origin2
point2XY           = lift2 P.point2XY
point2Polar        = lift2 P.point2Polar
point2XYCoordsB    = lift1 P.point2XYCoords
point2PolarCoordsB = lift1 P.point2PolarCoords
distance2          = lift2 P.distance2
distance2Squared   = lift2 P.distance2Squared
linearInterpolate2 = lift3 P.linearInterpolate2
p2v		   = lift1 P.p2v
v2p		   = lift1 P.v2p
mirrorX		   = lift1 P.mirrorX
mirrorY		   = lift1 P.mirrorY
mirrorXY	   = lift1 P.mirrorXY
(.+^)              = lift2 (P..+^)
(.-^)              = lift2 (P..-^)
(.-.)              = lift2 (P..-.)


-- These ones return pairs, so you don't have to pairBSplit

point2XYCoords, point2PolarCoords :: Point2B -> (RealB,RealB)

point2XYCoords    = pairBSplit . point2XYCoordsB
point2PolarCoords = pairBSplit . point2PolarCoordsB
