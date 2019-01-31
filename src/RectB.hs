-- Rectangular region behaviors

module RectB ( RectB
             , rectFromCorners, rectFromCenterSize
             , rectLL, rectUR, rectLR, rectUL
             , rectCenter, rectSize
             , intersectRect, expandRect, rectContains
             ) where

import qualified Rect as R
import Behavior
import Point2B (Point2B)
import Vector2B (Vector2B)

type RectB = Behavior R.Rect

rectFromCorners    :: Point2B -> Point2B  -> RectB
rectFromCenterSize :: Point2B -> Vector2B -> RectB
intersectRect      :: RectB   -> RectB    -> RectB
rectContains       :: RectB   -> Point2B  -> BoolB
expandRect         :: RealB   -> RectB    -> RectB

rectLL, rectUR, rectLR, rectUL :: RectB -> Point2B
rectCenter :: RectB -> Point2B
rectSize   :: RectB -> Vector2B


rectFromCorners    = lift2 R.rectFromCorners
rectFromCenterSize = lift2 R.rectFromCenterSize
intersectRect      = lift2 R.intersectRect
rectContains       = lift2 R.rectContains
expandRect         = lift2 R.expandRect

rectLL     = lift1 R.rectLL
rectUR     = lift1 R.rectUR
rectLR     = lift1 R.rectLR
rectUL     = lift1 R.rectUL
rectCenter = lift1 R.rectCenter
rectSize   = lift1 R.rectSize
