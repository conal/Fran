-- Rectangular region behaviors

module RectB where

import qualified Rect as R
import Behavior
import Point2B (Point2B)

type RectB = Behavior R.Rect

rectLLUR :: Point2B -> Point2B -> RectB
intersectRect :: RectB -> RectB -> RectB

rectLLUR      = lift2 R.rectLLUR
intersectRect = lift2 R.intersectRect
