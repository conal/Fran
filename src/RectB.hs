-- Rectangular region behaviors

module RectB (RectB, rectFromCorners, rectFromCenterSize
             , intersectRect
             ) where

import qualified Rect as R
import Behavior
import Point2B (Point2B)
import Vector2B (Vector2B)

type RectB = Behavior R.Rect

rectFromCorners    :: Point2B -> Point2B  -> RectB
rectFromCenterSize :: Point2B -> Vector2B -> RectB

intersectRect   :: RectB -> RectB -> RectB

rectFromCorners    = lift2 R.rectFromCorners
rectFromCenterSize = lift2 R.rectFromCenterSize
intersectRect      = lift2 R.intersectRect
