-- Behavior vectors
--
-- Last modified Wed Jul 09 09:06:11 1997

module Vector2B where

import qualified Vector2 as V
import Behavior

type Vector2B = Behavior V.Vector2

xVector2           = constantB V.xVector2
yVector2           = constantB V.yVector2
vector2XY          = lift2 V.vector2XY
vector2Polar       = lift2 V.vector2Polar
vector2XYCoords    = lift1 V.vector2XYCoords
vector2PolarCoords = lift1 V.vector2PolarCoords

-- Vector space operations lifted in VectorSpaceB.hs

-- These ones return pairs, so you don't have to pairBSplit

vector2XYCoords', vector2PolarCoords' :: Vector2B -> (RealB,RealB)

vector2XYCoords'    = pairBSplit . vector2XYCoords
vector2PolarCoords' = pairBSplit . vector2PolarCoords
