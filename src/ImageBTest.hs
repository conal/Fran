-- Simple test harness for Image behaviors
-- 
-- Last modified Tue Oct 08 14:19:32 1996
-- 
-- To have a go, run "disp i{j}" where j `elem` [1..].
-- Or run "disp allDemos" to see them all, and press <space> to go on to the
-- next item.
-- 
-- To do: Change all examples to be functions of a start time, rather than
-- assuming start time zero.  One benefit is that we can make temporal
-- compositions of the examples, including one super composition for
-- testing.  Another benefit is elimination of the huge value lists held
-- onto by top-level defs that include integrals.

module ImageBTest where

import RBMH
import qualified StaticTypes as S
import qualified ImageTest

import Random
import Win32 (timeGetTime)

import qualified ShowImageB

infixr 1 `seqImF`

disp imF = ShowImageB.disp (imF 0)

dispFps imF = disp (\t0 -> fpsImage t0 `over` imF t0)

-- Put these next two defs elsewhere:

-- An image of the fps behavior
fpsImage = withColor yellow . renderedText . simpleText . showB . fps

demos =   [ i0,  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8,  i9,
	   i10, i11, i12, i13, i14, i15, i16, i17, i18, i19,
	   i20, i21, i22, i23, i24, i25, i26, i27, i28, i29,
	   i30, i31, i32, i33, i34, i35, i36, i37 ]
-- Doesn't work.  Why??  (Try just "disp i1 >> disp i2".)
dispAll' = sequence (map disp demos)

seqImF :: (Time -> ImageB) -> (Time -> ImageB) -> (Time -> ImageB)

(imF `seqImF` imF') t0 =
  imF t0 `untilB` (keyPress `suchThat` (\(ch,_) -> ch == ' ')) t0
		    +=> \ t1 _ -> imF' t1

allDemos =
  foldr seqImF
	(\t0 ->
	 translate2 (vector2Polar 0.5 time) *%
	 withColor yellow
	   (renderedText (simpleText (lift0 "That's all folks!"))))
	demos

i0 t0 = translate2 (vector2XY (-1) 0) *%
	withColor green
	   (renderedText (simpleText (lift0 "Press <space> for next demo")))

i1 t0 = withColor red $
	uscale2 (0.75 + 0.25 * cos (5*time)) *%
	circle 

i2 t0 = translate2 (vector2Polar 0.7 time) *%
	uscale2 0.1 *%
	i1 t0

i3 t0 = translate2 (vector2Polar (time'/15) (sqr time')) *%
	 uscale2 0.1 *%
	 withColor red circle
      `over`
	 withColor (rgb (abs (cos time)) (abs (sin (2*time))) 0.5) (i1 t0)
        where
	  sqr x = x*x
	  time' = time - lift0 t0

i4 t0 =
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


i5 t0 =
  translate2 (vector2XY (sin time / 5) 0) *%
        renderedText (simpleText (lift0 "Click me"))
  `over`
     withColor (i5' red blue t0) (i1 t0)
 where
  i5' c1 c2 t0 =
   c1 `untilB` lbp t0 +=> \ t1 _ -> i5' c2 c1 t1


-- spinning ellipse
i6 t0 = withColor green $ rotate2 (3*time) *% ellipse (vector2XY 1 0.5)

-- Follow the mouse

i7 t0 = translate2 (mouse t0 `pointMinusPoint2` origin2) *%
	uscale2 0.2 *%
	withColor yellow circle

-- i8 t0 = withColor red (renderedText (simpleText (lift0 "nothing here")))

i8 t0 = withColor green $ renderedText (simpleText (strB t0))
	where
	  strB t0 = waiting    `untilB` keyPress t0 ==> \ (ch, release) ->
		    lift0 [ch] `untilB` release     *=> strB
	  waiting = lift0 "press a key"
		  
  

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

i9 t0 = smoothGrowShrink (withColor red $ uscale2 0.5 *% circle) t0

i10 t0 = smoothGrowShrink (lift0 ImageTest.i17) t0

i11 t0 = smoothGrowShrink (lift0 ImageTest.i18) t0

-- This one gives a control stack overflow.  Why??
iBlowStack t0 =
  smoothGrowShrink i12' t0
  where
    i12'    = unitBBoxed2 (turning `over` square)
    turning = rotate2 time *% uscale2 (1 / sqrt 2) *% i12'

-- An eye watching the mouse cursor.  Given the transform that maps the
-- eye to world (mouse) coordinates.  This sort of thing will be better
-- handled by "IImages" in a future version.

mouseWatcher :: Transform2B -> Time -> ImageB

mouseWatcher xf t0 =
  let
      localMousePos = inverse2 xf *% (mouse t0)
      lookAngle     = sndB (point2PolarCoords localMousePos)
      iris          = translate2 (vector2Polar 0.35 lookAngle) *%
                      uscale2 0.1                              *%
                      withColor blue circle
      eye           = iris `over` (withColor white (uscale2 0.5 *% circle))
  in
      xf *% eye


i12 t0 = mouseWatcher (translate2 (vector2XY 0.4 0.2)) t0

i13 t0 = mouseWatcher (translate2 (vector2Polar 0.3 time) `compose2`
		      scale2 (1 + 0.2 * sin time)) t0

-- Group watch

i14 t0 = foldl1 over (map watcher [0::Radians, 2*pi/5 .. 2*pi-0.01])
	 where
	   watcher angle =
	     mouseWatcher (translate2 (vector2Polar 0.7 (lift0 angle))
			   `compose2` uscale2 0.5) t0


-- Wiggly group watch

wigglyWatchers n t0 =
  foldl1 over (map watcher [0::Radians, 2*pi/n .. 2*pi-0.01])
  where
    watcher angle =
      mouseWatcher (translate2 (vector2Polar 0.7 angle')
                    `compose2` uscale2 0.5) t0
      where
        angle' = lift0 angle + cos (lift0 angle + 5*time) / (lift0 n)

i15 t0 = wigglyWatchers 5 t0


-- Being followed

chasingWatcher t0 =
  follower
  where
    seed1 = unsafePerformIO timeGetTime
    seed2 = seed1 + 2543
    x: y: _ =  randomDoubles seed1 seed2
    follower =
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3) t0
      where
       eyePos = startPos `pointPlusVector2` integral eyeVel t0
       eyeVel = integral eyeAcc t0
       eyeAcc = (mouse 0 `pointMinusPoint2` eyePos) `addVector`
                (dragFactor `scaleVector` eyeVel)
       dragFactor = -1
       -- x,y are in [0,1]
       startPos =  uscale2 1.5 *%
                   translate2 (vector2XY (-0.5) (-0.5)) *%
                   point2XY (lift0 x) (lift0 y)

i16 t0 = chasingWatcher t0

{-

chasingWatchers n =
  foldl1 over (zipWith follower (take n (randomDoubles seed1 seed2))
                                (take n (randomDoubles seed2 seed1)))
  where
  seed1 = unsafePerformIO timeGetTime
  seed2 = seed1 + 2543
  follower x y =
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3) t0
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
      mouseWatcher (translate2 eyePos `compose2` uscale2 0.3) t0
        `over` chasers (n-1) startPts' eyePos
      where
       eyePos = startPt `pointPlusVector2` integral eyeVel t0
       eyeVel = integral eyeAcc t0
       eyeAcc = (followPt `pointMinusPoint2` eyePos) `addVector`
                (dragFactor `scaleVector` eyeVel)
       dragFactor = -1

-- Use "i17 0"

i17 t0 = chasingWatchers' 4 t0


i18 t0 = rotate2 (time/5) *% lift0 (ImageTest.lotus 8 4)

-- Accumulate snapshots of a simple animation.

i19 t0 = anim `over` stepping t0 emptyImage
	 where
	   anim = withColor (hsl (180 * sin time) 0.5 0.5) (i2 t0)

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

stringTrail str =
 map (\ c -> withColor yellow $ renderedText (simpleText (lift0 [c]))) str

i20 t0 = motion_tracker 0.1 m1 (stringTrail "Time Transformation")
 where
  m1 = (uscale2 0.9 *% vector2XY (cos (3*time)) (sin (5*time)))
        `timeTransform` (time/3)

-- Time transformation of the mouse doesn't work well.  Come up with a
-- better representation for external events.

mouse_tracker t0 =
  motion_tracker 0.3 (mouse t0 `pointMinusPoint2` origin2)

i21 t0 = mouse_tracker t0 (stringTrail "RBMH")

i22 t0 = mouse_tracker t0 (take 13 (repeat (uscale2 0.1 *% circle)))




-- Bouncing ball examples.

-- 1D bounce path

bounce1 minVal maxVal x0 dx0 ddx t0 =
  start t0 (x0,dx0)
  where
    start t0 (x0,dx0) =
      x `untilB` bounce +=> start

      where
        x  = lift0  x0 + integral dx          t0
        dx = lift0 dx0 + integral (lift0 ddx) t0
        reciprocity = 0.8

        bounce :: Event (RealVal, RealVal)  -- new x and dx

        bounce = (collide `snapshot` pairB x dx)
                    ==> \ ((),(xHit,dxHit)) -> (xHit, - reciprocity * dxHit)

        collide = predicate (x <=* lift0 minVal &&* dx<=*0 ||*
                             x >=* lift0 maxVal &&* dx>=*0) t0

-- 2D bounce path

bounce2 (S.Point2XY  xMin yMin) (S.Point2XY  xMax yMax)
        (S.Point2XY  x0   y0  ) (S.Vector2XY dx0  dy0 )
        (S.Vector2XY ddx  ddy )
        t0 =
  point2XY x y
  where
    x = bounce1 xMin xMax x0 dx0 ddx t0
    y = bounce1 yMin xMax y0 dy0 ddy t0


-- Single bouncy ball

bounceFrame =
 polyline (map (\ i -> point2Polar sqrt2 (pi/4 + lift0 i * pi/2)) [0 .. 4])
 where
  sqrt2 = sqrt 2

bouncyBall t0 =
  translate2 (bounce2 (S.point2XY (-1+radius) (-1+radius))
		      (S.point2XY ( 1-radius) ( 1-radius))
		      S.origin2 (S.vector2XY 4.0 2.0)
		      (S.vector2XY 0 (-1.8)) t0) *%
  uscale2 (lift0 radius) *% withColor green circle
  where radius = 0.1


i23 t0 = bouncyBall t0 `over` bounceFrame

lotsOfBounces t0 =
  balls t0 `over` bounceFrame
  where
    balls t0 =
      emptyImage `untilB` lbp t0 +=> \ tBP _ ->
      balls tBP `over` (bouncyBall tBP `untilB`
                          timeIs (tBP+5) -=> emptyImage)

i24 t0 = lotsOfBounces t0

noverlay :: [ImageB] -> ImageB
noverlay = foldl (over) emptyImage
 
slowTime = 0.1*time

kaleido :: (Behavior Double -> Point2B)
        -> ImageB
kaleido f =
  (rotate2 (pi*sin slowTime)) *% (
  noverlay
     (zipWith (withColor)
	      colours
	      (map (\ x -> (rotate2 x) *% i) rads)))
  where
   colours = cycle [red,blue,green,yellow,lightBlue]
   rads    = map (lift0) [0,pi/4..2*pi]
   i       = polygon ls
   ls      = map f rads

i25 t0 = kaleido star
 where
  star v =
    point2XY
      (cos  (v        *sin (v*slowTime) + v*slowTime))
      (abs (sin  (slowTime *cos (v*slowTime) - v*slowTime)))

i26 t0 = kaleido star
 where
  star v =
      point2XY 
        (abs (sin (v*slowTime*sin (v*slowTime) + slowTime)))
        (abs (cos (  slowTime*sin (v*slowTime) + slowTime)))

chunksOf :: Int -> [a] -> [[a]]
chunksOf 0 ls = []
chunksOf n ls =
 case splitAt n ls of
   ([],_) -> []
   (x,xs) -> x:chunksOf n xs

translateT :: Time -> Behavior a -> Behavior a
translateT t b = timeTransform b (lift1 (+t) time)

i27 t0 =
  withColor 
     colours
     (foldl1
         (over)
	 (map i (chunksOf 5 [0,pi/18..2*pi])))
  where
   colours = stronger (abs (sin time)) black
   i ls    = polygon  (map (\ t -> translateT (negate t) 
					      pt) ls)
   pt      = point2XY (sin (2*time)) (cos (2*time))
   idx ls  = lift1 (\ idx -> ls!!(round idx))

triangle :: Point2B
         -> Point2B
         -> Point2B
         -> ImageB
triangle p1 p2 p3 = polyline [p1,p2,p3,p1]

i28 t0 =
  (rotate2 (sndB (point2PolarCoords (mouse t0)))) *% i
  where
   i =
    (triangle (point2XY 0.5 (-0.5)) 
	      (point2XY (-0.5) (-0.5)) 
	      (point2XY 0 0.5))

i29 t0 = 
 let
  i26' = i26 t0
 in
 i26'                                  `over` 
 (translateT 1 ((uscale2 0.8)  *% i26'))

i30 t0 = 
 noverlay (map (\ x -> translateT x i) [0,0.4..4])
 where
  p1 = (point2XY (-1/3) (cos time))
  p2 = (point2XY ( 1/3) (sin time))
  i = bezier 
	(point2XY (-1) 0)
	p1
	p2
	(point2XY 1 0)

i31 t0 = i
 where
  i = polygon [
	(point2XY (-sin time) (integral (cos time) t0)),
	(point2XY (-1/3) (cos time)),
	(point2XY (1/3) (sin time)),
	(point2XY (sin (pi-time)) 0)]

i32 t0 = 
 (translate2 p1 *% (uscale2 0.05 *% withColor red  circle)) `over`
 (translate2 p2 *% (uscale2 0.05 *% withColor blue circle)) `over`
 i
 where
  p0 = point2XY (-1) 0
  p1 = mouse 0
  p2 = translateT (-2) p1
  p3 = point2XY 1 0
  i  = bezier p0 p1 p2 p3 

mingle [] ls = ls
mingle ls [] = ls
mingle (x:xs) (y:ys) = x:y:mingle xs ys

i33 t0 =
  (rotate2 (pi*sin time')) *% i
  where
   rads    = mingle [0,pi/7..pi] [-pi/2,(-5*pi/14)..pi/2]
   i       = withColor blue (polygon ls)
   time'   = 0.2*time
   ls =
    map (\ x -> point2XY (abs (0.5*cos ((sin time')*x+x*time'/pi)))
			 (sin ((sin (x*time'))+0.4*time')))
        (map (lift0) rads)

i34 t0 = withColor red (lift1 (S.polygon) (Behavior (\ _ -> ls)))
 where 
  ls = koch [S.point2XY (-1) 0, S.point2XY 1 0]

i35 t0 = withColor red (lift1 (S.polygon) (Behavior (\ _ -> ls)))
 where 
  ls     = koch (map (S.point2Polar 1) [0,2*pi/3..2*pi+0.1])
	
koch :: [S.Point2] -> [[S.Point2]]
koch ls =
 let
  ls' = map2 (koch1) ls
 in
 ls'++koch (last ls')

map2 :: (a -> a -> [a]) -> [a] -> [[a]]
map2 f (x:y:ls) = map (f x y ++) (ls : map2 f (y:ls))
map2 _ _ = []

koch1 p0 p4 = [p0,p1,p2,p3,p4]
 where
  dp  = p4   `S.pointMinusPoint2` p0
  dp' = S.scaleVector 0.33 dp
  p1  = p0  `S.pointPlusVector2`  dp'
  p2  = p1  `S.pointPlusVector2`  (S.rotate2 (pi/3)) S.*% dp'
  p3  = p4  `S.pointMinusVector2` dp'

i36 t0 =
  let
   pt1 = point2XY (cos time) (sin time)
   pt2 = point2XY (sin time) (cos time)
  in
  bezier (mouse t0) 
	 (translateT (-pi/2) pt2)
	 pt1
	 (translateT (3*pi/2) pt2)

-- Based on a similar function by Gary Shu Ling

viewStretch :: String -> Time -> ImageB

viewStretch bitmapFilename t0 =
  case S.importBitmap bitmapFilename of
      S.EmptyImage -> emptyImage
      im@(S.Bitmap size _) ->
        scale2 (vector2XY (wWidth / lift0 iWidth)
			  (wHeight / lift0 iHeight))
	 *% lift0 im
	where
	  (wWidth, wHeight) = pairBSplit (vector2XYCoords (viewSize t0))
	  (iWidth, iHeight) = S.vector2XYCoords size


i37 = viewStretch "..\\Media\\frog.bmp"


iViewSize =
  (translate2 (vector2XY (-0.9) 0) *%) . withColor yellow .
  renderedText . simpleText . showB . viewSize


iBlowStack2 t0 =
  translate2 (vector2XY (integral 0 t0) 0) *% circle 
  `timeTransform` (time*440)


