-- Simple rectangular regions

module Rect ( Rect (..)
            , rectFromCorners, rectFromCenterSize
            , rectLL, rectUR, rectLR, rectUL
            , rectCenter, rectSize
            , intersectRect, expandRect, rectContains
            ) where

import BaseTypes
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

rectLL, rectUR, rectLR, rectUL :: Rect -> Point2
rectLL (RectLLUR ll _) = ll
rectUR (RectLLUR _ ur) = ur

rectLR (RectLLUR (Point2XY minx miny) (Point2XY maxx maxy)) =
 Point2XY maxx miny
rectUL (RectLLUR (Point2XY minx miny) (Point2XY maxx maxy)) =
 Point2XY minx maxy


rectFromCenterSize :: Point2 -> Vector2 -> Rect
rectFromCenterSize p v = RectLLUR (p .-^ halfV) (p .+^ halfV)
 where
   halfV = v ^/ 2

rectCenter :: Rect -> Point2
rectCenter (RectLLUR (Point2XY minx miny) (Point2XY maxx maxy)) =
  Point2XY (minx `av` maxx) (miny `av` maxy)
 where
   a `av` b = (a + b)/2

rectSize :: Rect -> Vector2
rectSize (RectLLUR (Point2XY minx miny) (Point2XY maxx maxy)) =
  Vector2XY (maxx - minx) (maxy - miny)

intersectRect :: Rect -> Rect -> Rect

RectLLUR (Point2XY minx1 miny1) (Point2XY maxx1 maxy1) `intersectRect`
   RectLLUR (Point2XY minx2 miny2) (Point2XY maxx2 maxy2) =
  RectLLUR (Point2XY minx miny) (Point2XY maxx maxy)
 where
   minx = minx1 `max` minx2 ; miny = miny1 `max` miny2
   maxx = maxx1 `min` maxx2 ; maxy = maxy1 `min` maxy2

instance Transformable2 Rect where
 xf@(Transform2 _ _ 0) *% RectLLUR ll ur =
   rectFromCorners (xf *% ll) (xf *% ur)
 _ *% _ = error "Rotation not yet supported on Rect"


-- Expand a rect about its center
expandRect :: RealVal -> Rect -> Rect
expandRect s rect =
  rectFromCenterSize (rectCenter rect)
                     (s *^ rectSize rect)


-- Containment, considering the box to be closed
rectContains :: Rect -> Point2 -> Bool
rectContains (RectLLUR (Point2XY minx miny) (Point2XY maxx maxy))
             (Point2XY x y) =
  minx <= x && x <= maxx  &&  miny <= y && y <= maxy
