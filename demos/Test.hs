-- Module for testing Fran

module Test where

import Fran
import qualified Win32
import qualified StaticTypes as S
import IOExts ( trace )


-- The test cases

donutSurface :: HDDSurface
donutSurface = bitmapDDSurface "../Media/donuts.bmp"

bounceBuffer, engineBuffer, monsoonBuffer, planeBuffer :: HDSBuffer
bounceBuffer  = waveDSBuffer "../Media/bounce.wav"
engineBuffer  = waveDSBuffer "../Media/sengine.wav"
monsoonBuffer = waveDSBuffer "../Media/monsoon2.wav"
planeBuffer   = waveDSBuffer "../Media/plane.wav"

donutFlipBook :: HFlipBook
donutFlipBook = flipBook donutSurface 64 64 0 0 5 6

donut :: Vector2B -> RealB -> RealB -> ImageB

donut motionB scaleB pageB =
  move motionB $
  stretch scaleB  $
  soundImage (bufferSound bounceBuffer True) `over`
  flipImage donutFlipBook pageB


linearDonut :: RealVal -> RealVal -> RealVal -> RealVal
            -> User -> ImageB

linearDonut velX velY scaleRate pageRate u =
  donut (vector2XY (x0 + dt * constantB velX) (y0 + dt * constantB velY))
        scale
        (dt * constantB pageRate)
  where
   dt = userTime u
   scale = 1 + dt * constantB scaleRate
   x0 = -1
   y0 = x0


donut0, donut1, donut2, donut3, donut4, donut5, twoDonuts, threeDonuts
  :: User -> ImageB

donut0 u = flipImage donutFlipBook 0


donut0'  u = stretch 1 $ flipImage donutFlipBook 0
donut0'' u = stretch (userTime u) $ flipImage donutFlipBook 0

donut1 = linearDonut 0.40 0.35 0.0  50
donut2 = linearDonut 0.50 0.45 0.2  70
donut3 = linearDonut 0.45 0.40 0.5 100

donut4 u = donut (vector2Polar 1 dt) 1
                 (10 * dt)
  where dt = userTime u

donut5 u = donut (vector2Polar 1 dt) sc
                 (30 * sin (dt/3))
  where dt = userTime u
        sc = 1 + 0.4 * sin dt 

donut6 u =  --trace ("donut6': t0 = " ++ show (userStartTime u) ++ "\n") $
            donut (vector2Polar dist ang) sc
                  (20 * dt)
            `untilB` stop -=> emptyImage
  where dt   = 3 * userTime u
        ang  = dt
        dist = 1 - 0.05 * dt
        sc   = dist
        stop = --userTimeIs 7 u
               predicate (sc <* 0.1) u

-- Like donut4, but spirals inward
donut7 u = donut (vector2Polar (1/(1+dt/2)) dt) 1
                 (10 * dt)
  where dt = userTime u


donut8 u =
  move (vector2XY 0 1) (showBIm dt)
     `over`
  (donut0 u `untilB` ev -=> emptyImage)
 where
   ev = predicate (dt >=* 5) u
   dt = userTime u

donut8a u = donut0 u `untilB` userTimeIs 5 u -=> emptyImage
donut8b u = donut0 u `untilB` predicate (userTime u >=* 5) u -=> emptyImage


--donut9 u = donut0 u `untilB` lbp' u -=> emptyImage

--lbp' u = traceE "lbp" TraceOccsE (lbp u)
--rbp' u = traceE "rbp" TraceOccsE (rbp u)



twoDonuts u =
 donut2 u `over` donut1 u

threeDonuts u =
 donut3 u `over`
 (twoDonuts u `untilB` userTimeIs 2 u -=> emptyImage)

wordy0 u = stringBIm (constantB "Hello world!")

wordy1 u =
 (withColor blue $
  move (vector2Polar (1 / (1+dt/2)) (- dt)) $
  stretch 2 $
  showBIm dt) `over`
 donut7 u
  where
   dt = userTime u

wordy2 u =
 ( withColor color $
   move (vector2Polar (1 / (1+dt/2)) (- dt)) $
   stretch scale   $
   turn angFrac  $
   textImage text) `over`
 donut7 u
  where
   dt = userTime u
   scale = 1.5 + sin dt
   angFrac = dt / 3
   color = colorHSL h 0.5 0.5
   h = pi * (1 + sin dt)
   text = simpleText (showB dt)

turnWordy angFrac u =
 turn angFrac $ textImage (simpleText (constantB "A String to render"))

wordy3 u = turnWordy (sin (userTime u) * pi / 2) u

wordy4 u =
  stretch 2 $
  withColor blue  (turnWordy (  sin (userTime u) * pi / 2) u) `over`
  withColor green (turnWordy (- sin (userTime u) * pi / 2) u)


-- simplification of lotsODonuts

-- This test trips on the commented-out "seq" in afterE.  (Event.hs)
seqD9 u = 
 soFar `untilB` nextE e ==> \ e ->
 soFar `untilB` nextE e -=> emptyImage
  where
    soFar = emptyImage
    e = alarmE (userStartTime u) 2 `afterE_` u

seqD1 u = b `untilB` (userTimeIs 2.1111 u `afterE_` b)
 where
   b = donut1 u

lotsODonuts u = accumB' over (donut6 u) another
 where
  another = alarmUE 1.3 u ==> donut6

-- Alternative to accumB that avoids the need for afterE.  Less
-- polymorphic and slightly different semantics, agrees when the op is
-- associative and ident is an identity.
accumB' :: GBehavior bv => (bv -> bv -> bv) -> bv -> Event bv -> bv
accumB' op ident = loop
 where
   loop e = ident `untilB` withRestE e ==> \ (b',e') ->
            b' `op` loop e'


alarmUE dt u = alarmE (userStartTime u) dt `afterE_` u

typing u = stretch 2 $
           stringBIm $
           stepper "start typing"
                   (scanlE snoc "" (charPressAny u))
 where
   snoc s c = s ++ [c]                  -- reverse cons

-- From the tutorial

importKidHiRes name =
  -- The small ones came from shrinking the big ones by 35% (without
  -- smoothing, which messes up color-keying).
  stretch 0.35 (
   importBitmap ("../Media/" ++ name ++ "Big.bmp"))

importKidLoRes name =
  importBitmap ("../Media/" ++ name ++ "Small.bmp")

-- Which to use?
importKid = importKidHiRes


leftRightCharlotte = moveXY wiggle 0 charlotte

charlotte = importKid "Charlotte"
pat       = importKid "Pat"

upDownPat = moveXY 0 wiggle pat

charlottePatDance = leftRightCharlotte `over` upDownPat 

patHorses = moveXY wiggle waggle pat `over` horses

patMouse u = move (mouseMotion u) pat

patMoves u = moveXY (-1 + atRate 1 u) 0 pat


-- Fun with time shifting

shifter u = marker p2 red  `over`
            marker p3 blue `over`  curve
 where
  curve = bezier p1 p2 p3 p4
  p1    = point2XY (-1) 0
  p2    = mouse u
  p3    = later 2 p2
  p4    = point2XY 1 0
  marker p col =
    move (p .-. origin2) $
    stretch 0.1          $
    withColor col        $
    circle

-- Sound tests

bounceS = bufferSound bounceBuffer True

s0 u = soundImage $ pan (10 * sin (userTime u)) bounceS
   

snd0 u = donut0 u `over`
         soundImage (bufferSound bounceBuffer True)

-- Left-to-right panning
snd1 u = move (vector2XY (sin (userTime u)) 0) (snd0 u)

snd1' u = move (mouseMotion u) (snd0 u)

-- shrink/fade
snd2 u = stretch (sin (userTime u / 5)) (snd0 u)

snd3' u = stringBIm (constantB "Move the mouse around")

snd3 u = stringBIm msg `over`
         soundImage (pitch y (volume x (bufferSound planeBuffer True)))
 where
  msg = constantB "Move the mouse around"
  (x, y) = vector2XYCoords (mouse u .-. point2XY (-1) (-1.5))

snd4 u = accum u `untilB` nextUser_ (keyPress Win32.vK_ESCAPE) u ==> snd4
 where
  accum u = emptyImage `untilB` nextUser_ (keyPress Win32.vK_SPACE) u ==> \ u' ->
            accum u' `over` donut1 u'

snd5 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` nextUser_ (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = silence `untilB` nextUser_ (keyPress Win32.vK_SPACE) u ==> \ u' ->
            bufferSound bounceBuffer True `mix` accum u'

snd6 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` nextUser_ (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = accumB mix silence (addSound u)
  addSound u = withTimeE (bounceButton u)  ==> bounce
  bounceButton u = nextUser_ (keyPress Win32.vK_SPACE) u
  bounce (u',te) = --trace ("bounce at " ++ show (userStartTime u', te) ++ "\n") $
                   bufferSound bounceBuffer True


growHowTo :: User -> ImageB

growHowTo u = moveXY 0 (- winHeight / 2 + 0.1) $
              withColor yellow $
              stringBIm messageB
  where
    winHeight = snd (vector2XYCoords (viewSize u))
    messageB = selectLeftRight "Use mouse buttons to control pot's spin"
               "left" "right" u


grow, growExp :: User -> RealVal -> RealB

grow u x0 = size
 where 
  size = constantB x0 + atRate rate u 
  rate = bSign u

-- Yipes!! This one is blows the stack :-(

growExp u x0 = size
 where 
  size = constantB x0 + atRate rate u 
  rate = bSign u * size

{-
bSign :: User -> RealB

bSign u = 
 0 `untilB` lbp u ==> nonZero (-1) lbr .|. 
            rbp u ==> nonZero 1    rbr
 where
  nonZero :: RealB -> (User -> Event User) -> User -> RealB
  nonZero r stop u = 
   r `untilB` stop u ==> bSign
-}


selectLeftRight :: a -> a -> a -> User -> Behavior a

selectLeftRight none left right u = notPressed u
 where
  notPressed u =
   constantB none `untilB` 
     nextUser_ lbp u ==> pressed left  lbr .|. 
     nextUser_ rbp u ==> pressed right rbr 

  pressed x stop u = 
   constantB x `untilB` nextUser_ stop u ==> notPressed

bSign :: User -> RealB

bSign = selectLeftRight 0 (-1) 1


jumpPat u = buttonMonitor u `over`
            moveXY (bSign u) 0 pat

growPat u = buttonMonitor u `over`
            stretch (grow u 1) patHiRes

growExpPat u = buttonMonitor u `over`
               stretch (growExp u 1) pat

patHiRes = stretch 0.35 $ importBitmap ("../Media/PatBig.bmp")

buttonMonitor u =
  moveXY 0 (- height / 2 + 0.25) $
  withColor white $
  stretch 2       $
  stringBIm (selectLeftRight "(press a button)" "left" "right" u)
 where
   (width,height) = vector2XYCoords (viewSize u)


-- Becky art


wildcat, horses :: ImageB

wildcat = importBitmap "../Media/wildcat.bmp"

horses = importBitmap "../Media/horses.bmp"


frolic u =
  move (mouseMotion u) (stretch wcSize wildcat) `over`
  horses
 where
  wcSize = 0.3


-- Testing mutually reactive behaviors


-- A "self reactive" behavior.

iRecReact u = stretch x (donut0 u)
 where
  x = userTime u `untilB` predicate (x >=* 1) u -=> 1

-- Mutually reactive.  Works
iMut1 u = d x1 0.5 `over` d x2 (-0.5)
 where
  d x y = moveXY x y (donut0 u)
  x1 = -1 + userTime u `untilB` predicate (x2 >=* 1) u -=> 1
  x2 = -1 + userTime u `untilB` predicate (x1 >=* 1) u -=> 1

-- Throw in integration.  Works
iMut2 u = d x1 0.5 `over` d x2 (-0.5)
 where
  d x y = moveXY x y (donut0 u)
  x1 = -1 + atRate   1  u `untilB` predicate (x2 <=* -1) u -=>  1
  x2 =  1 + atRate (-1) u `untilB` predicate (x1 >=*  1) u -=> -1

-- Throw in a snapshot.  Works
iMut3 u = d x1 0.5 `over` d x2 (-0.5)
 where
  d x y = moveXY x y (donut0 u)
  x1 = -1 + atRate   1  u `untilB`
       predicate (x2 <=* -1) u `snapshot_` x1 ==> constantB 
  x2 =  1 + atRate (-1) u `untilB`
       predicate (x1 >=*  1) u `snapshot_` x2 ==> constantB 

-- Factor.  Works
iMut4 u = d x1 0.5 `over` d x2 (-0.5)
 where
  d x y = moveXY x y (donut0 u)
  x1 = m (-1) 0.5 (x2 <=*)
  x2 = m 1 (-0.5) (x1 >=*)
  m x0 dx stop = x
   where x = x0 + atRate dx u `untilB`
              predicate (stop x0) u `snapshot_` x
                                     ==>        constantB 

-- Try with collisions.  Works, but doesn't contain velocity part of
-- collision test, so shortly after a collision, there may be false
-- collision.  The "right" fix is probably to detect the exact collision
-- time.  Meanwhile, the behavior is very sensitive to the rebound number,
-- which should be 2.0 for perfect elasticity.
iMut5 u = ball r1 x1 red `over` ball r2 x2 blue
 where
   ball r x c = moveXY x 0 $ stretch r $ withColor c $ circle
   x1 = linearBump (-1) 0.5 x2
   x2 = linearBump 1 (-0.5) x1
   linearBump x0 dx0 obst = x
     where
       x  = x0  + atRate dx u
       dx = dx0 + sumE impulse
       impulse = collide `snapshot_` dx ==> (* (-2.0))
       collide = predicate (magnitude (x - obst) <* minDist) u
   r1 = 0.1; r2 = 0.2
   minDist = r1 + r2

-- Add velocity part.  Consolidate radius, position, and velocity
-- into a single object.  Works, but the "~" is crucial!
iMut6 u = ball o1 red `over` ball o2 blue
 where
   ball (r,x,dx) c = moveXY x 0 $ stretch r $ withColor c $ circle
   o1 = linearBump (0.1,-1,0.5) o2
   o2 = linearBump (0.2,1,-0.5) o1
   linearBump (r,x0,dx0) ~(r',x',dx') = (r,x,dx)
     where
       x  = x0  + atRate dx u
       dx = dx0 + sumE impulse
       relX  =  x -  x'
       relDX = dx - dx'
       impulse = collide `snapshot_` dx ==> (* (-1.9))
       collide = predicate (magnitude relX <* r+r' &&*
                            dot relDX relX <* 0) u

-- ## Note: the dot product test is wrong.  We shouldn't be using just
-- relX, but rather something involving minDist as well.  Maybe we need to
-- use "derivative (magnitude relX) >* 0", which means they are moving
-- apart. ##

sumE :: Num a => Event a -> Behavior a
sumE ev = stepper 0 (scanlE (+) 0 ev)

-- Now let's throw in a reactive event, making the balls disappear
-- when they hit.  Add a disappearance event.  This one blows the
-- control stack :(.  I now understand what's going on here, but not
-- how to fix it.  The culprit is untilB on events.  Both die events
-- need to know a lower bound for the other before giving a lower
-- bound for itself.
iMut7 u = ball o1 red `over` ball o2 blue
 where
   ball (r,x,die) c = (moveXY x 0 $ stretch r $ withColor c $ circle)
                       `untilB` die -=> emptyImage
   o1 = linearBoom (0.1,-1,0.5) o2
   o2 = linearBoom (0.2,1,-0.5) o1
   linearBoom (r,x0,dx0) ~(r',x',die') = (r,x,die)
    where
      x   = x0 + atRate dx0 u
      die = predicate (magnitude (x-x') <* r+r') u
             `untilB` die' -=> trace "died\n" neverE

{-

-- Experimenting with a fix.  Apply the following function to an event to
-- make one that gets checked regularly.
checkE :: Event a -> User -> Event a
checkE (Event possOccs) u =
  Event (loop possOccs Nothing updateTimes)
 where
   loop ((te,mb):pos') mbLast (t:ts')
     | t > te  =  
     
   updateTimes = [ t | (t, mb) <- possOccsOf u, isJust mb ]

-}
 

-- This version avoids the problem by moving the until die' into the
-- predicate.  I don't know how to extend this hack to the situation in
-- Roids, in which one object can be replaced by a set of them.
iMut8 u = ball o1 red `over` ball o2 blue
 where
   ball (r,x,die) c = (moveXY x 0 $ stretch r $ withColor c $ circle)
                       `untilB` die -=> emptyImage
   o1 = linearBoom (0.1,-1,0.5) o2
   o2 = linearBoom (0.2,1,-0.5) o1
   linearBoom (r,x0,dx0) ~(r',x',die') = (r,x,die)
    where
      x   = x0 + atRate dx0 u
      die = predicate (alive' &&* magnitude (x-x') <* r+r') u
      alive' = trueB `untilB` die' -=> falseB


-- GHC type-chokes, thinking that the VectorSpaceB operations are on Scalar

-- Mutual forces.  This one works and is fun to watch.
iMut9 u = --stretch (1 / (1 + userTime u)) $
          overs (map render os)
 where
   -- The objects
   os = map go starts
   go (mot0, vel0, r, col) = o
    where
      o = (mot, vel, r, col)
      -- Each object is acted on by all objects.
      (mot,vel) = newton mot0 vel0 r (sumVSB (map (force o) os))
   force = springForce -- gravForce
   -- Spring law, with drag
   springForce (mot,vel,_,_) (mot',_,_,_) = k *^ (mot' - mot) ^-^ d *^ vel
   -- Gravitational force
   gravForce (mot, _, m, _) (mot', _, m', _) =
      condB (distSquared ==* 0) zeroVector (fMag *^ fDir)
     where
       fDir = normalize (- relMot) 
       fMag = (bigG * constantB(m*m')) / distSquared
       distSquared = magnitudeSquared relMot
       relMot = mot ^-^ mot'
   -- F = m a
   newton mot0 vel0 m f = (mot,vel)
    where
      mot = constantB mot0 ^+^ atRate vel u
      vel = constantB vel0 ^+^ atRate acc u
      acc = f ^/ constantB m
   render (mot, _, r, col) =
     move mot $ stretch (constantB r) $ withColor (constantB col) $ circle
   -- Initial object configurations
   starts = [ ( S.vector2Polar 0.4 ang0
              , -- S.zeroVector
                S.vector2Polar 0.25 (ang0 + pi/2)
              , 0.2 / fromInt i
              , S.colorHSL ang0 0.5 0.5)
            | i <- [1 .. n], let ang0 = fromInt i * s ]
   n = 4    -- # of bodies
   k = 0.5  -- spring constant
   d = 0.05 -- drag constant
   s = 2 * pi / fromInt n  :: RealVal
   bigG = 1

sumVSB :: VectorSpace a => [Behavior a] -> Behavior a
sumVSB = foldl (^+^) zeroVector



-- Works fine

iTst6 u = withColor red (stretch x (donut0 u))
 where
  x = userTime u `untilB` userTimeIs 6 u `snapshot` x -=> 1

iTst7 u = withColor red (stretch x (donut0 u))
 where
  x = atRate dx u `untilB` userTimeIs 5 u `snapshot` x -=> 1
  dx = 0.2 :: Behavior RealVal

iTst8 u = withColor red (stretch x (donut0 u))
 where
  x = atRate dx u `untilB` userTimeIs 3 u `snapshot_` x ==> constantB
  dx = 0.3 :: Behavior RealVal

iTst9 u = withColor red (stretch x (donut0 u))
 where
  x = atRate dx u `untilB` predicate (x>=*1) u `snapshot` x -=> 1
  dx = 0.3 :: Behavior RealVal

iTst10 u = withColor red (stretch x (donut0 u))
 where
  x = atRate dx u `untilB` predicate (x>=*1) u `snapshot_` x ==> constantB
  dx = 0.3 :: Behavior RealVal


uPeriod u = showBIm (updatePeriod u)

-----------------------------------------------------------------
-- test 2D stuff
-----------------------------------------------------------------

l0 = line (point2XY (-1) (-1)) (point2XY 1 1)

l1 u = stretch wiggle circle
l2 u = polygon [point2XY 1 1, point2XY (- abs wiggle) 0,
		      point2XY 1 wiggle]
l3 u = line (point2XY 1 1) (point2XY wiggle wiggle)
l4 u = slower 2 $ stretch (wiggleRange 2 4) $
       stringBIm (constantB text !!* roundB (wiggleRange 0 6))
 where
   text = words "Where Do You Want To Go Today?"
   strs = map stringIm text
l5 u = regularPolygon 6
l6 u = turn (userTime u / 15) $ star 3 10
l7 u = turn (userTime u / 3) $
       star (roundB (10 * wiggle)) 20
l8 u = turn (userTime u / 3) $
       slower 5 $ star 5 (roundB (wiggleRange 5 35))

pause :: (User -> ImageB) -> User -> ImageB
pause f u = f u `untilB`
	    lbp u ==> const 1		-- const 1 is redundant
	    `snapshot_` time 
	    `afterE` u ==> \ (t, u') ->
	    f u' `timeTransform` constantB t `untilB` 
	    lbp u' ==> const (f u') 
	    `afterE_` u' ==>		-- const (...) is redundant too
	    pause f

l9 = pause l2


-- Crop tests.

flower = stretch 0.4 (importBitmap "../Media/rose medium.bmp")

cropFlower rect = crop rect (stretch 3 flower)

crop1 u = cropFlower (rectFromCorners ll ur)
 where
   ll = point2XY (-0.5) (-0.5) .+^ vector2Polar 0.2 (2*time)
   ur = point2XY ( 0.5) ( 0.5) .+^ vector2Polar 0.3 (3*time)

crop2 u = cropFlower (rectFromCenterSize (mouse u) (vector2XY 1 1))

crop2' = cropMagnify 1 0.5 (stretch 3 flower)

crop3 = cropFlower . rubberBandRect

crop4 u = cropMagnify 3 1 flower u `over` flower

crop5 u = cropMagnify 6 1 imB u `over` imB
 where
   imB = stretch 0.3 (stringIm str)
   -- ## If this string is too long, the DDraw CreateSurface call dies,
   -- taking the process with it. Clipping synthetic images before
   -- rendering should fix the problem. ##
   str = "Animation can be fun."
         --++ "  Just say what the animation is and let Fran do the rest."

crop6 u = cropMagnify 3 1 imB u `over` imB
 where
   imB = donut4 u

rubberBandRect :: User -> RectB
rubberBandRect u = rectFromCorners ll ur
 where
   mousePos = mouse u
   ll = stepper S.origin2 (lbp u `snapshot_` mousePos)
   ur = condB (leftButton u) mousePos lastReleasePos
   lastReleasePos = stepper S.origin2 (lbr u `snapshot_` mousePos)

cropMagnify :: RealB -> RealB -> ImageB -> User -> ImageB
cropMagnify factor size imB u =
  frame  `over`
  crop (rectFromCenterSize mousePos (vector2XY size size)) (
       stretchAtPoint factor mousePos imB
       -- Use solid black background, so that the transparent parts of the
       -- image are no longer transparent.
       `over` withColor black solidImage
       )
 where
   mousePos = mouse u
   halfSize = size / 2
   frame' = withColor white $
            polyline [ mousePos .+^ halfSize *^ vector2XY x y
                     | (x,y) <- last corners : corners ]
   -- The following should be equivalent, but looks much better, because
   -- it moves at every frame (~50Hz) rather than ever update (~10Hz).
   frame = withColor white           $
           move (mousePos .-. origin2) $
           stretch halfSize          $
           polyline [ point2XY x y | (x,y) <- last corners : corners ]
   corners = [(-1,-1),(1,-1),(1,1),(-1,1)]

tstFrame u = withColor white $
             polygon [ pos .+^ 0.25 *^ vector2XY x y
                      | (x,y) <- last corners : corners ]
 where
   pos = mouse u
   --pos = point2Polar 0.5 time
   --pos = point2XY (userTime u / 5) 0
   corners = [(-1,-1),(1,-1),(1,1),(-1,1)]

stretchAtPoint :: Transformable2B bv => RealB -> Point2B -> bv -> bv
stretchAtPoint factor point = (uscaleAtPoint2 factor point *%)

uscaleAtPoint2 :: RealB -> Point2B -> Transform2B
uscaleAtPoint2 factor point =
  translate2 motion    `compose2`
  uscale2    factor    `compose2`
  translate2 (-motion)
 where
   motion = point .-. origin2


-- Test the effects stuff.  Use tryEffects for these ones

tryEffects imF = do
  w <- displayEx imF
  setWindowTextA w "Fran with effects"
  eventLoop w

effects1 u = (patMouse u,  const $
                             lbp u -=> putStrLn "Hey!"
                         .|. rbp u -=> putStrLn "Watch it!")

effects2 u = (circle, changeTitle)
  where
    changeTitle w = press ==> ("Pressed " ++) ==> setWindowTextA w 
    press = (lbp u -=> "left" .|. rbp u -=> "right") ==> (++ " button")
        .|. charPressAny u ==> (: " key")


-- setViewSize not working

-- effects3 u = (star 5 9, changeSize)
--   where
--     changeSize w = newSize ==> setSize w
--     size = stepper (S.vector2XY 2 2) newSize
--     newSize = changeE `snapshot` size ==> uncurry ($)
--     changeE = keyPressAny u `assocE`
--                   [ (Win32.vK_LEFT , changeW (/2))
--                   , (Win32.vK_RIGHT, changeW (*2))
--                   , (Win32.vK_DOWN , changeH (/2))
--                   , (Win32.vK_UP   , changeH (*2)) ]
--     changeW f (S.Vector2XY w h) = (S.Vector2XY (f w) h)
--     changeH f (S.Vector2XY w h) = (S.Vector2XY w (f h))

--     setSize w size = do setViewSize w size
--                         putStrLn ("New size: " ++ show size)

-- Simultaneous windows.  Use displayUs to test

imFs1 = [wordy2, patMouse, crop4]
