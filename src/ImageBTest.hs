-- Simple test harness for Image behaviors
-- 
-- Last modified Mon Sep 16 15:05:14 1996
-- 
-- To have a go, run disp i{j} where j `elem` [1..]
-- 
-- To do: Change all examples to be functions of a start time, rather than
-- assuming start time zero.  One benefit is that we can make temporal
-- compositions of the examples, including one super composition for
-- testing.  Another benefit is elimination of the huge value lists held
-- onto by top-level defs that include integrals.

module ImageBTest where

import RBMH

import Random
import Win32 (timeGetTime)

import qualified ShowImageB

disp = ShowImageB.disp


-- Doesn't work.  Why??  (Try just "disp i1 >> disp i2".)
dispAll = sequence (map disp [
            i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14,
            i15, i16, i17 0, i18, i19, i20, i21 ])

i1 = withColor red $
     uscale2 (0.75 + 0.25 * cos (5*time)) *%
     circle 

i2 = translate2 (vector2Polar 0.7 time) *%
     uscale2 0.1 *%
     i1

i3 = translate2 (vector2Polar (time/15) (time*time)) *%
     uscale2 0.1 *%
     withColor red circle
  `over`
     withColor (rgb (abs (cos time)) (abs (sin (2*time))) 0.5) i1

i4 = foldl1 over
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


i5 = translate2 (vector2XY (sin time / 5) 0) *%
        renderedText (simpleText (lift0 "Click me"))
  `over`
     withColor (i5' red blue 0) i1
 where
  i5' c1 c2 t0 =
   c1 `untilB` lbp t0 +=> \ t1 _ -> i5' c2 c1 t1


-- spinning ellipse
i6 = withColor green $ rotate2 (3*time) *% ellipse (vector2XY 1 0.5)

-- Follow the mouse

i7 = translate2 (mouse 0 `pointMinusPoint2` origin2) *%
     uscale2 0.2 *%
     withColor yellow circle

-- Variable growth rate.  Left button decreases and right increases.

i8 = smoothGrowShrink (withColor red circle) 0
     where
        smoothGrowShrink :: ImageB -> Time -> ImageB
        smoothGrowShrink anim t0 =
          uscale2 (exp (integral (changingRate 0 t0) t0)) *% anim
          where
            changingRate r0 t0 =
              lift0 r0 `untilB` minusOneOrOne t0 +=> \ t1 increase ->
                 changingRate (r0 + increase) t1
            minusOneOrOne t0 = lbp t0 -=> -1  .|.  rbp t0 -=> 1



-- For the next three, shrink/grow while left/right button down

smoothGrowShrink anim t0 =
  uscale2 (exp (integral (changingRate t0) t0)) *% anim
  where
    changingRate t0 =
      0 `untilB` setRate t0 ==> \ (rate, stop) ->
        lift0 rate `untilB` stop +=> \ t1 _ -> changingRate t1
    -- Occurs when a button is pressed, yielding the new rate and
    -- a stop event.
    setRate t0 =
      (lbp t0 ==> \ lRelease -> (-1, lRelease)) .|.
      (rbp t0 ==> \ rRelease -> ( 1, rRelease))

i9 = smoothGrowShrink (withColor red $ uscale2 0.5 *% circle) 0

import qualified ImageTest

i10 = smoothGrowShrink (lift0 ImageTest.i17) 0

i11 = smoothGrowShrink (lift0 ImageTest.i18) 0

-- This one gives a control stack overflow.  Why??
iBlowStack =
  smoothGrowShrink i12' 0
  where
    i12'    = unitBBoxed2 (turning `over` square)
    turning = rotate2 time *% uscale2 (1 / sqrt 2) *% i12'

-- An eye watching the mouse cursor.  Given the transform that maps the
-- eye to world (mouse) coordinates.  This sort of thing will be better
-- handled by "IImages" in a future version.

mouseWatcher :: Transform2B -> ImageB

mouseWatcher xf =
  let
      localMousePos = inverse2 xf *% (mouse 0)
      lookAngle     = sndB (point2PolarCoords localMousePos)
      iris          = translate2 (vector2Polar 0.35 lookAngle) *%
                      uscale2 0.1                              *%
                      withColor blue circle
      eye           = iris `over` (withColor white (uscale2 0.5 *% circle))
  in
      xf *% eye


i12 = mouseWatcher (translate2 (vector2XY 0.4 0.2))

i13 = mouseWatcher (translate2 (vector2Polar 0.3 time) `compose2`
                    scale2 (1 + 0.2 * sin time))

-- Group watch

i14 = foldl1 over (map watcher [0::Radians, 2*pi/5 .. 2*pi-0.01])
      where
        watcher angle =
          mouseWatcher (translate2 (vector2Polar 0.7 (lift0 angle))
                        `compose2` uscale2 0.5)
              

-- Wiggly group watch

wigglyWatchers n =
  foldl1 over (map watcher [0::Radians, 2*pi/n .. 2*pi-0.01])
  where
    watcher angle =
      mouseWatcher (translate2 (vector2Polar 0.7 angle')
                    `compose2` uscale2 0.5)
      where
        angle' = lift0 angle + cos (lift0 angle + 5*time) / (lift0 n)

i15 = wigglyWatchers 5


-- Being followed

chasingWatcher =
  follower
  where
    seed1 = unsafePerformIO timeGetTime
    seed2 = seed1 + 2543
    x: y: _ =  randomDoubles seed1 seed2
    follower =
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3)
      where
       eyePos = startPos `pointPlusVector2` integral eyeVel 0
       eyeVel = integral eyeAcc 0
       eyeAcc = (mouse 0 `pointMinusPoint2` eyePos) `addVector`
                (dragFactor `scaleVector` eyeVel)
       dragFactor = -1
       -- x,y are in [0,1]
       startPos =  uscale2 1.5 *%
                   translate2 (vector2XY (-0.5) (-0.5)) *%
                   point2XY (lift0 x) (lift0 y)

i16 = chasingWatcher

{-

chasingWatchers n =
  foldl1 over (zipWith follower (take n (randomDoubles seed1 seed2))
                                (take n (randomDoubles seed2 seed1)))
  where
  seed1 = unsafePerformIO timeGetTime
  seed2 = seed1 + 2543
  follower x y =
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3)
      where
       eyePos = startPos `pointPlusVector2` integral eyeVel 0
       eyeVel = integral eyeAcc 0
       eyeAcc = (mouse 0 `pointMinusPoint2` eyePos) `addVector`
                (dragFactor `scaleVector` eyeVel)
       dragFactor = -1
       -- x,y are in [0,1]
       startPos =  uscale2 1.5 *%
                   translate2 (vector2XY (-0.5) (-0.5)) *%
                   point2XY (lift0 x) (lift0 y)

-- Oops: all the watchers converge, so n>1 is not very interesting. :-(  I
-- guess they'd have to also avoid each other somehow.  Or: have a chain
-- of them, in which the first one follows the mouse and the later ones
-- follow their predecessor, as in chasingWatchers' below
-}


chasingWatchers' n t0 =
  chasers n
          (zipWith startPos (randomDoubles seed1 seed2)
                            (randomDoubles seed2 seed1))
          (mouse t0)
  where
    seed1 = unsafePerformIO timeGetTime
    seed2 = seed1 + 2543

    startPos x y = -- x,y are in [0,1]. Transform to [-1.5,1.5]
                   uscale2 1.5 *%
                   translate2 (vector2XY (-0.5) (-0.5)) *%
                   point2XY (lift0 x) (lift0 y)

    chasers 0 _                   _      = emptyImage

    chasers n (startPt:startPts') followPt = 
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3)
        `over` chasers (n-1) startPts' eyePos
      where
       eyePos = startPt `pointPlusVector2` integral eyeVel t0
       eyeVel = integral eyeAcc t0
       eyeAcc = (followPt `pointMinusPoint2` eyePos) `addVector`
                (dragFactor `scaleVector` eyeVel)
       dragFactor = -1

-- Use "i17 0"

i17 t0 = chasingWatchers' 4 t0


i18 = rotate2 (time/5) *% lift0 (ImageTest.lotus 8 4)

-- Accumulate snapshots of a simple animation.

i19 = anim `over` stepping 0 emptyImage
      where
        anim = withColor (hsl (180 * sin time) 0.5 0.5) i2

        stepping t0 im0 =
          im0 `untilB`
            (lbp t0 `snapshot` anim) ==> snd ==> lift0 ==> (`over` im0)
             +=> stepping

-- Trailing motion

motion_tracker spc motion trail =
 -- withColor (rgb (abs (sin (2*time))) 0.8 0.5) $
 foldl1 over
  (zipWith
    (\ tShift im ->
       (translate2 motion *% im) `timeTransform` (time - lift0 tShift))
    [0.0,spc..]
    trail)

-- Turn a string into a list of simple text images.

stringTrail str = map (\ c -> withColor yellow $ renderedText (simpleText (lift0 [c]))) str

i20 = motion_tracker 0.1 m1 (stringTrail "Time Transformation")
 where
  m1 = (uscale2 0.9 *% vector2XY (cos (3*time)) (sin (5*time)))
        `timeTransform` (time/3)

-- Time transformation of the mouse doesn't work well.  Come up with a
-- better representation for external events.

mouse_tracker = motion_tracker 0.3 (mouse 0 `pointMinusPoint2` origin2)

i21 = mouse_tracker (stringTrail "RBMH")

i22 = mouse_tracker (take 13 (repeat (uscale2 0.1 *% circle)))


