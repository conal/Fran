-- Simple rectangular regions

module Rect (Rect (..), rectFromCorners, rectFromCenterSize
            , intersectRect
            ) where

import Point2
import Vector2
import VectorSpace
import Transform2

-- Rectangular region for cropping.
-- The datatype constructor has lower-left and upper-right
data Rect = RectLLUR Point2 Point2 deriving Show

rectFromCorners :: Point2 -> Point2 -> Rect
rectFromCorners (Point2XY x1 y1) (Point2XY x2 y2) =
  RectLLUR (Point2XY (x1 `min` x2) (y1 `min` y2))
           (Point2XY (x1 `max` x2) (y1 `max` y2))

rectFromCenterSize :: Point2 -> Vector2 -> Rect
rectFromCenterSize p v = RectLLUR (p .-^ halfV) (p .+^ halfV)
 where
   halfV = v ^/ 2

intersectRect :: Rect -> Rect -> Rect

RectLLUR (Point2XY ll1x ll1y) (Point2XY ur1x ur1y) `intersectRect`
    RectLLUR (Point2XY ll2x ll2y) (Point2XY ur2x ur2y)  =
  RectLLUR (Point2XY llx lly) (Point2XY urx ury)
 where
   llx = ll1x `max` ll2x ; lly = ll1y `max` ll2y
   urx = ur1x `min` ur2x ; ury = ur1y `min` ur2y

instance Transformable2 Rect where
 xf@(Transform2 _ _ 0) *% RectLLUR ll ur =
   rectFromCorners (xf *% ll) (xf *% ur)
 _ *% _ = error "Rotation not yet supported on Rect"