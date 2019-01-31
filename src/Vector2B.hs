-- Behavior vectors
--
-- Last modified Tue Oct 07 13:23:28 1997

module Vector2B where

import qualified Vector2 as V
import Behavior

type Vector2B = Behavior V.Vector2

xVector2            = constantB V.xVector2
yVector2            = constantB V.yVector2
vector2XY           = lift2 V.vector2XY
vector2Polar        = lift2 V.vector2Polar
vector2XYCoordsB    = lift1 V.vector2XYCoords
vector2PolarCoordsB = lift1 V.vector2PolarCoords
rotateVector2       = lift2 V.rotateVector2

-- Vector space operations lifted in VectorSpaceB.hs

-- These ones return pairs, so you don't have to pairBSplit

vector2XYCoords, vector2PolarCoords :: Vector2B -> (RealB,RealB)

vector2XYCoords    = pairBSplit . vector2XYCoordsB
vector2PolarCoords = pairBSplit . vector2PolarCoordsB
