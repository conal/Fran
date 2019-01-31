-- Simple test harness for Image behaviors
-- 
-- Last modified Sun Sep 08 16:57:13 1996
-- 
-- To have a go, run disp i{j} where j `elem` [1..]

module ImageBTest where

import Behavior
import Event
import Vector2B
import Point2B
import ColorB
import TextB
import Transform2B
import ImageB

import Integral
import Until
import PrimInteract
import Interaction

import qualified ShowImageB

disp = ShowImageB.disp

i1 =
 withColor red $
 uscale2 (0.75 + 0.25 * cos (5*time)) *%
 circle 

i2 = translate2 (vector2Polar 0.7 time) *%
     uscale2 0.1 *%
     i1

i3 =
    translate2 (vector2Polar (time/15) (time*time)) *%
    uscale2 0.1 *%
    withColor red circle
  `over`
    withColor (rgb (abs (cos time)) (abs (sin (2*time))) 0.5) i1

i4 =
 foldl1 over
  [withColor royalBlue                     $
   translate2 (vector2XY 0.50 0)           *%
   rotate2 (pi * sin time)                 *%
   importBitmap "..\\Media\\Xguy.bmp",

   withColor yellow                        $
   translate2 (vector2Polar 0.6 time)      *%
   uscale2 (0.1 - 0.08 * cos time)         *%
     circle,

   withColor red                            $
   translate2 (vector2Polar 0.6 (-time))    *%
   uscale2 (0.1 + 0.06 * cos time)          *%
     circle,

   withColor (rgb 0.5 0.7 (abs (sin time))) $
   uscale2 0.5                              *%
   circle]


i5 = 
     translate2 (vector2XY (sin time / 5) 0) *%
        renderedText (simpleText (lift0 "Click me"))
  `over`
     withColor (i5' red blue 0) i1
 where
  i5' c1 c2 t0 =
   c1 `untilB` lbp t0 +=> \ t1 _ -> i5' c2 c1 t1


-- Follow the mouse

i7 = translate2 (mouse 0 `pointMinusPoint2` origin2) *%
     uscale2 0.2 *%
     withColor yellow circle

{-

i6 =
 i6' 1 0
 where
  i6' f t = 
   uscale2 f *% i1  `untilB`  ((primLBP t +=> \ t p -> 
   (translate2 (lift0 (Point2.point2Vec p)) *%
   uscale2 f *% i1) `untilB` primLBR t +=> \ t _ -> 
    i6' (f+0.2) t)
        .|. 
   (primRBP t +=> \ t _ -> i6' (f-0.2) t))

i7 = 
 i7' (vector2 10 10) 0
 where
  i7' v t0 = 
    translate (lift0 v) i1 `untilB` primMousePos t0 +=> \ t1 p -> 
    i7' (Point2.point2Vec p) t1

-}

-- Variable growth rate.  Left button decreases and right increases.

i8 = smoothGrowShrink (withColor red circle) 0
     where
        smoothGrowShrink :: ImageB -> Time -> ImageB
        smoothGrowShrink anim t0 =
          uscale2 (1 + integral (changingRate 0 t0) t0) *% anim
          where
            changingRate r0 t0 =
              lift0 r0 `untilB` minusOneOrOne t0 +=> \ t1 increase ->
                 changingRate (r0 + increase) t1
            minusOneOrOne t0 = lbp t0 -=> -1  .|.  rbp t0 -=> 1



-- For the next three, shrink/grow while left/right button down

i9 = smoothGrowShrink (withColor red $ uscale2 0.5 *% circle) 0

smoothGrowShrink anim t0 =
  uscale2 (1 + integral (changingRate t0) t0) *% anim
  where
    changingRate t0 =
      0 `untilB` setRate t0 ==> \ (rate, stop) ->
        lift0 rate `untilB` stop +=> \ t1 _ -> changingRate t1
    -- Occurs when a button is pressed, yielding the new rate and
    -- a stop event.
    setRate t0 =
      (lbp t0 ==> \ lRelease -> (-1, lRelease)) .|.
      (rbp t0 ==> \ rRelease -> ( 1, rRelease))



import qualified ImageTest

i10 = smoothGrowShrink (lift0 ImageTest.i17) 0

i11 = smoothGrowShrink (lift0 ImageTest.i18) 0

-- This one gives a control stack overflow.  Why??
i12BlowStack =
  smoothGrowShrink i12' 0
  where
    i12'    = unitBBoxed2 $
              turning `over` square
    turning = rotate2 time *% uscale2 m *% i12'
    m = 1 / sqrt 2

