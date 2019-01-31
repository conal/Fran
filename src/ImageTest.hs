-- Simple test harness for static Image values and their display
-- 
-- Last modified Sat Sep 07 23:25:52 1996
-- To try these out, run disp i{j} where j `elem` [1..] 

module ImageTest where

import Image
import Text
import Color
import ShowImage
import Vector2
import Point2
import Transform2

disp im = showImage im


i1 = circle

i2 = square

i3 = line (point2XY (-1) (-1)) (point2XY 1 1) `over` i2

star vertices skip =
  polyline (map (\i -> point2Polar 1 (2 * pi * i * skip / vertices))
                [0 .. vertices])

i4 = star 13 5

i5 = rectangle (vector2XY 0.7 1.3)

i6 = ellipse  (vector2XY 1.0 0.7)

i7 = bezier (point2XY (-0.7) (-0.7))
            (point2XY   0      0.7 )
            (point2XY (-0.7)   0.7 ) `over` i2

 
i10 = importBitmap "..\\Media\\Xguy.bmp"

i11 = withColor yellow i4

i12 = withColor red i2

i13 = uscale2 0.5 *% i12

i14 = translate2 (vector2XY 0.2 0.2) *% i12

i15 = i11 `over` i12

i16 = withColor white $ renderedText (simpleText "Hello, World")



-- Infinitely recursive images with automatic cut-off during drawing

i17 =
  i17' red blue
  where
    i17' c1 c2 = unitBBoxed2 $
                 uscale2 0.7 *% (i17' c2 c1)
                 `over` withColor c1 circle


i18 =
  i18' where
       i18' = unitBBoxed2 $
              uscale2 0.5 *% i18' `over`
              withColor yellow  square      `over`
              withColor blue circle



lotus :: Int -> Image

lotus n = uscale2 0.7 *% (withColor red all)
  where
    all = unitBBoxed2 $
          (rotate2 theta     *%
           uscale2 shrinkage *%
           all) `over`
          ring

    ring = foldl1 over
             (map (\ i -> rotate2 (2 * fromInt i * theta)
                            *% basePart)
                  [0 .. n-1])

    theta = pi / fromInt n  -- half of the angle subtended by the square
    c = cos theta
    s = sin theta
    shrinkage = c - s   -- size ratio of inner to outer layerrs
    basePart = 
      translate2 (vector2XY c 0) *%
      uscale2 s                  *%
      rotate2 (pi/4)             *%
      square

-- Warning: these guys are slow!

i19 = lotus 7
i20 = lotus 10
