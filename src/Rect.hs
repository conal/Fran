-- Simple rectangular regions

module Rect where

import Point2
import Transform2

-- Rectangular region for cropping
data Rect = RectLLUR Point2 Point2 deriving Show

rectLLUR = RectLLUR

intersectRect :: Rect -> Rect -> Rect

RectLLUR (Point2XY ll1x ll1y) (Point2XY ur1x ur1y) `intersectRect`
    RectLLUR (Point2XY ll2x ll2y) (Point2XY ur2x ur2y)  =
  RectLLUR (Point2XY llx lly) (Point2XY urx ury)
 where
   llx = ll1x `max` ll2x ; lly = ll1y `max` ll2y
   urx = ur1x `min` ur2x ; ury = ur1y `min` ur2y

instance Transformable2 Rect where
 xf@(Transform2 _ _ 0) *% RectLLUR ll ur = RectLLUR (xf *% ll) (xf *% ur)
 _ *% _ = error "Rotation not yet supported on Rect"