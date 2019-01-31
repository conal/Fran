-- Behavior vectors
--
-- Last modified Sat Sep 07 23:35:06 1996

module Vector2B where

import qualified Vector2 as V
import Behavior

type Vector2B = Behavior V.Vector2

xVector2           = lift0 V.xVector2
yVector2           = lift0 V.yVector2
vector2XY          = lift2 V.vector2XY
vector2Polar       = lift2 V.vector2Polar 
vector2XYCoords    = lift1 V.vector2XYCoords
vector2PolarCoords = lift1 V.vector2PolarCoords

-- Vector space operations lifted in VectorSpaceB.hs
