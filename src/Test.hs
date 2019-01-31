-- Module for testing Fran
--
-- Last modified Tue Oct 07 14:43:42 1997

module Test where

import Fran
import qualified Win32
import Trace



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
  soundImage (bufferSound bounceBuffer) `over`
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
   x0 = constantB $ (- fromInt initialWindowSize / 2) / screenPixelsPerLength
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


donut9 u = donut0 u `untilB` lbp' u -=> emptyImage

lbp' u = traceE "lbp" TraceOccsE (lbp u)
rbp' u = traceE "rbp" TraceOccsE (rbp u)



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
   turnLeft angle  $
   textImage text) `over`
 donut7 u
  where
   dt = userTime u
   scale = 1.5 + sin dt
   angle = dt / 3
   color = colorHSL h 0.5 0.5
   h = 180 * (1 + sin dt)
   text = simpleText (showB dt)

wordy3 angle u =
 turnLeft angle $ textImage (simpleText (constantB "A String to render"))

wordy4 u = wordy3 (sin (userTime u) * pi / 2) u

wordy5 u =
  stretch 2 $
  withColor blue  (wordy3 (  sin (userTime u) * pi / 2) u) `over`
  withColor green (wordy3 (- sin (userTime u) * pi / 2) u)


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

lotsODonuts u = accumB over (donut6 u) another
 where
  another = (alarmE (userStartTime u) 1.2 `afterE_` u) ==> donut6

-- From the tutorial

importHead name =
  importBitmap ("../Media/" ++ name ++ " head black background.bmp")

{-
importBMP name = flipImage book 0
 where
  book = flipBook surf w h 0 0 1 1
  surf = bitmapDDSurface ("../Media/" ++ name ++ " head black background.bmp")
  (w,h) = getDDSurfaceSize surf
-}

leftRightCharlotte = moveXY wiggle 0 charlotte

charlotte = importHead "charlotte"
pat       = importHead "pat"

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
    smaller 10           $
    withColor col        $
    circle

-- Sound tests

snd0 u = donut0 u `over`
         soundImage (bufferSound bounceBuffer)

-- Left-to-right panning
snd1 u = move (vector2XY (sin (userTime u)) 0) (snd0 u)

snd1' u = move (mouseMotion u) (snd0 u)

-- shrink/fade
snd2 u = stretch (abs (sin (userTime u / 5))) (snd0 u)

snd3' u = stringBIm (constantB "Move the mouse around")

snd3 u = stringBIm msg `over`
         soundImage (pitch y (volume x (bufferSound planeBuffer)))
 where
  msg = constantB "Move the mouse around"
  (x, y) = vector2XYCoords (mouse u .-. point2XY (-1) (-1.5))

snd4 u = accum u `untilB` nextUser (keyPress Win32.vK_ESCAPE) u ==> snd4
 where
  accum u = emptyImage `untilB` nextUser (keyPress Win32.vK_SPACE) u ==> \ u' ->
            accum u' `over` donut1 u'

snd5 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` nextUser (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = silence `untilB` nextUser (keyPress Win32.vK_SPACE) u ==> \ u' ->
            bufferSound bounceBuffer `mix` accum u'

snd6 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` nextUser (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = accumB mix silence (addSound u)
  addSound u = withTimeE (bounceButton u)  ==> bounce
  bounceButton u = nextUser (keyPress Win32.vK_SPACE) u
  bounce (u',te) = trace ("bounce at " ++ show (userStartTime u', te) ++ "\n") $
                   bufferSound bounceBuffer


--- 3D !!

-- Import an X mesh file to make a geometry.  Currently presumes that the
-- X file is a simple mesh.
importX :: String -> GeometryB
importX = meshG . meshBuilder

-- Stand back far enough to look at a unit sphere comfortably
defaultCamera = translate3 (vector3XYZ 0 0 (-5))

teapot, sphere :: GeometryB
-- teapot = meshGeometry (meshBuilder "../Media/tpot2.x")
teapot = uscale3 2 **% rotate3 xVector3 (-pi/2) **% importX "../Media/tpot2.x"
-- teapot = uscale3 0.005 **% importX "1701d.x"
--teapot = meshG (meshBuilder "tpot2.x")
sphere = importX "../Media/sphere1.x"

dispG g = disp $ \u -> renderGeometry (g u) defaultCamera

ig0 u = renderGeometry g defaultCamera
 where
   g = withColorG red $
       rotate3 yVector3 (userTime u) **%
       sphere

ig1 = withSpin potSpin1
ig2 = withSpin potSpin2
ig3 = withSpin potSpin3

withSpin :: (RealB -> User -> GeometryB) -> User -> ImageB

withSpin f u = growHowTo u  `over` geomImage
 where
   geomImage = renderGeometry (f (grow u 0) u) defaultCamera

spinPot :: ColorB -> RealB -> GeometryB

spinPot potColor potAngle =
  rotate3 yVector3 potAngle **% withColorG potColor teapot

potSpin1, potSpin2, potSpin3 :: RealB -> User -> GeometryB

potSpin1 angle u = spinPot red angle

potSpin2 potAngleSpeed u = spinPot potColor potAngle `unionG` light
 where
  light = rotate3 axis (pi/3) **%
          translate3 (vector3Spherical 2 (userTime u) 0) **%
          uscale3 0.1 **% (sphere `unionG` pointLightG)
  potColor = colorHSL (sin (userTime u / 3) * 180) 0.5 0.5
  potAngle = atRate potAngleSpeed u
  axis = yVector3
  -- axis = rotate3 zVector3 (userTime u) **% yVector3


potSpin3 potAngleSpeed u =
  translate3 (vector3Spherical 0.5 (- userTime u) 0) **%
    potSpin2 potAngleSpeed u

-- Simple moving image of rendered constant geometry
ig4 u = move (vector2Polar 0.5 (- userTime u)) $
       renderGeometry (withColorG red teapot) defaultCamera


-- Geometry switching.  As of 10/6/97, this guy doesn't work, because
-- emptyFrame is not really implemented.
g1 = gSphere `untilF` \u -> keyPressAny u -=> gPot
 where
   gSphere u = withColorG red $
               uscale3 (sin (userTime u)) **%
               rotate3 yVector3 (userTime u) **%
               sphere
   gPot    u = potSpin2 1 u



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
     nextUser lbp u ==> pressed left  lbr .|. 
     nextUser rbp u ==> pressed right rbr 

  pressed x stop u = 
   constantB x `untilB` nextUser stop u ==> notPressed

bSign :: User -> RealB

bSign = selectLeftRight 0 (-1) 1


jumpPat u = buttonMonitor u `over`
            moveXY (bSign u) 0 pat

growPat u = buttonMonitor u `over`
            stretch (grow u 1) pat

growExpPat u = buttonMonitor u `over`
               stretch (growExp u 1) pat



buttonMonitor u =
  moveXY 0 (-1) $
  withColor white $
  bigger 2 $
  stringBIm (selectLeftRight "(press a button)" "left" "right" u)

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

iRecReact u = withColor red (stretch x (donut0 u))
 where
  x = userTime u `untilB` predicate (x >=* 1) u -=> 1


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

l1 u = bigger wiggle circle
l2 u = polygon [point2XY 1 1, point2XY (- abs wiggle) 0,
		      point2XY 1 wiggle]
l3 u = line (point2XY 1 1) (point2XY wiggle wiggle)
l4 u = slower 2 $ bigger (wiggleRange 2 4) $
       stringBIm (lift0 text !!* roundB (wiggleRange 0 6))
 where
   text = ["Where", "Do", "You", "Want", "To", "Go", "Today?"]
   strs = map stringIm text
l5 u = regularPolygon 6
l6 u = turnLeft (userTime u / 15) $ star 3 10
l7 u = star (roundB (3 + wiggle)) (roundB (10 - wiggle))
