-- Mapping ImageB to sprite trees

module Spritify (
  disp,
  donutFlipBook
    ) where

import BaseTypes
import qualified StaticTypes as S
import Event
import Behavior
import BehaviorEvent
import Vector2B
import ColorB
import VectorSpaceB
import ImageB
import SoundB
import User
import Interaction
import IORef(Ref,newRef,getRef,setRef)
import Update
import Trace
import HSpriteLib
import ShowImageB

-- For examples
import Point2B
import GeometryB
import qualified Win32
import Integral
import Vector3B
import Transform3B
import TextB
import UtilsB


-- Internal and external representation for a list of sprite trees
type IETrees = ([UpdateTree], SpriteTreeChain)

emptyIETrees = ([], emptySpriteTreeChain)

spritify :: ImageB -> Time -> IO IETrees

spritify imb t0 =
  --putStrLn ("Starting to spritify " ++ show imb) >>
  spritifyRec Nothing zeroVector 1 imb t0 emptyIETrees

-- Make an UpdateTree list and a sprite chain.  Accumulate a list of
-- sprite trees *above* the current imageB and translation and scale
-- vector behaviors.  Note that the scale is applied before the
-- translation, i.e., move motionVec (scale scaleVec im)

spritifyRec :: Maybe ColorB -> Vector2B -> RealB -> ImageB
	    -> Time -> TransformIO IETrees

spritifyRec mbColor motion scale EmptyImage t0 =
  -- Nothing to augment.  Return same IETrees
  return


spritifyRec mbColor motion scale (FlipImage flipBook page) t0 =
 \ (pAbove, sAbove) ->
 do hFlipSprite <- newFlipSprite flipBook 0 0 1 1 0 sAbove
    --putStrLn "Made new flipSprite"
    motionRef <- newRef motion
    scaleRef  <- newRef scale
    pageRef   <- newRef page
    -- The color is ignored -- sorry.
    let
        update :: Time -> Time -> IO ()

        update t t' =
          updateBvrRefIO motionRef t $ \ mot  ->
          updateBvrRefIO scaleRef  t $ \ sc   ->
          updateBvrRefIO pageRef   t $ \ page ->
          --putStrLn ("Updating flipSprite: " ++ show (mot,sc,page)) >>
          updateFlipSprite hFlipSprite t' mot sc page
     in
        do update t0 t0
           return (UpdateIO update : pAbove , toSpriteTree hFlipSprite)

{-
spritifyRec mbColorB motionB scaleB (SyntheticImage surfGen) t0 =
 \ (pAbove, sAbove) ->
 do hSimpleSprite <- newSimpleSprite surf0 0 0 1 1 sAbove
    --putStrLn "Made new SimpleSprite"
    motionRef <- newRef motionB
    surfRef   <- newRef surfB
    let
        update :: Time -> IO ()

        update t t' =
          updateBvrRefIO motionRef t $ \ motion  ->
          updateBvrRefIO surfRef   t $ \ surf ->
          --putStrLn "Updating SimpleSprite" >>
          updateSimpleSprite hSimpleSprite t' motion surf
     in
        -- A mystery: if I put in the "update t0" below, then the Paint
        -- method bombs, saying DDERR_SURFACEBUSY, meaning "Access to the
        -- surface is refused because the surface is locked by another
        -- thread."
        do -- update t0
           return (UpdateIO update : pAbove , toSpriteTree hSimpleSprite)
 where
   surfB = surfGen mbColorB scaleB
   -- To do: phase out the need for surf0
   (surf0, _) = surfB `at` t0
-}

-- Will replace SyntheticImage
spritifyRec mbColorB motionB scaleB (SyntheticImageIO surfGenIO) t0 =
 \ (pAbove, sAbove) ->
 do motionRef <- newRef motionB
    surfGen   <- surfGenIO mbColorB scaleB -- init and make generator
    surf0     <- surfGen t0 t0
    hSimpleSprite <- newSimpleSprite surf0 0 0 1 1 sAbove
    --putStrLn "Made new SimpleSprite"
    let
        update :: Time -> Time -> IO ()

        update t t' =
          updateBvrRefIO motionRef t $ \ motion  ->
          surfGen t t'                >>= \ surf ->
          --putStrLn "Updating SimpleSprite" >>
          updateSimpleSprite hSimpleSprite t' motion surf
     in
        -- A mystery: if I put in the "update t0" below, then the Paint
        -- method bombs, saying DDERR_SURFACEBUSY, meaning "Access to the
        -- surface is refused because the surface is locked by another
        -- thread."
        do -- update t0
           return (UpdateIO update : pAbove , toSpriteTree hSimpleSprite)


spritifyRec mbColor motion scale (SoundI sound) t0 =
  spritifySound motion scale sound t0

spritifyRec mbColor motion scale (imb `Over` imb')  t0 =
  -- Spritify the upper and then the lower (see below for ".>>=")
  spritifyRec mbColor motion scale imb  t0  .>>=
  spritifyRec mbColor motion scale imb' t0


-- Use   move motion . stretch sc . stretch sc' ==
--	 move motion . stretch (sc * sc')

spritifyRec mbColor motion sc (Stretch sc' imb) t0 =
  spritifyRec mbColor motion (sc * sc') imb t0

-- Use   move motion . stretch sc . move motion' ==
--	 move motion . move motion'' . stretch sc  ==
--	 move (motion + motion'')    . stretch sc
--
-- where motion'' is as defined below

spritifyRec mbColor motion sc (Move motionLeng' imb) t0 =
  spritifyRec mbColor (motion + motion'') sc imb t0
  where
   --(motionX', motionY') = vector2XYCoords' motionPix
   --motion'' = vector2XY (sc * motionX') (sc * motionY')
   motion'' = sc *^ motionPix
   motionPix = (constantB screenPixelsPerLength) *^ motionLeng'

spritifyRec mbColor motion scale (WithColor colorInner imb) t0 =
  spritifyRec (mbColor ++ Just colorInner) motion scale imb t0


spritifyRec mbColor motion scale (imb `UntilI` e)   t0 = \ (pAbove, sAbove) ->
  -- Make a new sprite group with the results of spritifying imb.
  --putStrLn "Making sprite group" >>
  makeGroupMembers t0 imb            >>= \ (updateTrees, spriteChain) ->
  newSpriteGroup spriteChain sAbove  >>= \ group ->
  -- Make a PSpriteGroup whose event makes the new group members
  return ( UpdateGroup updateTrees (withTimeE e ==> updateGroup group) :
             pAbove ,
           toSpriteTree group )
  where
   makeGroupMembers t0 anim =
     spritifyRec mbColor motion scale anim t0 emptyIETrees
   updateGroup group (imb', t1) =
     makeGroupMembers t1 imb'  >>= \ (updateTrees', spriteChain') ->
     resetSpriteGroup group spriteChain' False >>
     return updateTrees'

-- Similar to spritify, but on SoundB.  Used from spritifyRec.
-- Takes translation and scale
spritifySound :: Vector2B -> RealB -> SoundB
              -> Time -> TransformIO IETrees

-- Extract initial pan and volume from motion and scale
spritifySound motion scale sndB = 
  spritifySoundRec scale x 1 sndB
  where
    -- The 0.3 is emperical
    x = 0.1 * fstB (vector2XYCoords motion)


-- Similar to spritifyRec, but on SoundB
spritifySoundRec :: RealB -> RealB -> RealB -> SoundB
                 -> Time -> TransformIO IETrees

spritifySoundRec vol pan pitch SilentS t0 =
  return

spritifySoundRec volB panB pitchB (BufferS buff) t0 =
 \ (pAbove, sAbove) ->
 do hSoundSprite <- newSoundSprite buff 1 0 1 sAbove
    --putStrLn "Made new SoundSprite" >>
    volRef   <- newRef volB
    panRef   <- newRef panB
    pitchRef <- newRef pitchB

    let
        update :: Time -> Time -> IO ()

        update t t' =
          updateBvrRefIO volRef   t $ \ vol   ->
          updateBvrRefIO panRef   t $ \ pan   ->
          updateBvrRefIO pitchRef t $ \ pitch ->
          --putStrLn "Updating soundSprite" >>
          updateSoundSprite hSoundSprite t' vol pan pitch
     in
        do update t0 t0
           return (UpdateIO update : pAbove , toSpriteTree hSoundSprite)

spritifySoundRec vol pan pitch (sound `MixS` sound') t0 =
  -- Spritify the upper and then the lower (see below for ".>>=")
  spritifySoundRec vol pan pitch sound  t0  .>>=
  spritifySoundRec vol pan pitch sound' t0

spritifySoundRec vol pan pitch (VolumeS v sound') t0 =
  spritifySoundRec (vol * v) pan pitch sound' t0

spritifySoundRec vol pan pitch (PitchS p sound') t0 =
  spritifySoundRec vol pan (pitch * p) sound' t0

-- Badly redundant with spritifyRec
spritifySoundRec vol pan pitch (snd `UntilS` e)   t0 = \ (pAbove, sAbove) ->
  -- Make a new sprite group with the results of spritifying snd.
  --putStrLn "Making sprite group" >>
  makeGroupMembers t0 snd            >>= \ (updateTrees, spriteChain) ->
  newSpriteGroup spriteChain sAbove  >>= \ group ->
  -- Make a UpdateGroup whose event makes the new group members
  return ( UpdateGroup updateTrees (withTimeE e ==> updateGroup group)
             : pAbove ,
           toSpriteTree group )
  where
   makeGroupMembers t0 anim =
     spritifySoundRec vol pan pitch anim t0 emptyIETrees
   updateGroup group (snd', t1) =
     makeGroupMembers t1 snd'  >>= \ (updateTrees', spriteChain') ->
     resetSpriteGroup group spriteChain' False >>
     return updateTrees'


updateFlipSprite :: HFlipSprite -> Time
		-> S.Vector2 -> RealVal -> Double 
                -> IO ()

updateFlipSprite hFlipSprite t (S.Vector2XY dx dy) sc page =
 do setGoalScale    (toSprite hFlipSprite) sc sc t
    setGoalPosition (toSprite hFlipSprite) dx dy t
    setGoalPage hFlipSprite page t

updateSimpleSprite :: HSimpleSprite -> Time
		-> S.Vector2 -> HDDSurface
                -> IO ()

updateSimpleSprite hSimpleSprite t (S.Vector2XY dx dy) surf =
 do setGoalPosition  (toSprite hSimpleSprite) dx dy t
    setSurface	     hSimpleSprite surf

{- This one is a SpriteLib primitive.  The others should be as well

updateSoundSprite hSoundSprite t vol pan pitch =
  return ()
 do setGoalScale    (toSprite hSoundSprite) sc sc t
    --putStrLn ("offset " ++ show (S.Vector2XY dx dy))
    setGoalPosition (toSprite hSoundSprite) dx dy t
-}

{- 

updateFlipSpriteB :: HFlipSprite -> Vector2B -> RealB -> RealB -> IOB ()

updateFlipSpriteB hFlipSprite motion scale page =
  --trace ("updateFlipSpriteB: motion at 10 is " ++ show (fst (motion `at` 10)) ++ "\n" )$
  lift4 (updateFlipSprite hFlipSprite)
	motion scale page

updateSimpleSpriteB :: HSimpleSprite -> Vector2B
		    -> SurfaceB -> IOB ()

updateSimpleSpriteB hSimpleSprite =
  lift3 (updateSimpleSprite hSimpleSprite)

updateSoundSpriteB :: HSoundSprite -> RealB -> RealB -> RealB -> IOB ()

updateSoundSpriteB hSoundSprite vol pan pitch =
  --trace ("updateSoundSpriteB: motion at 10 is " ++ show (fst (motion `at` 10)) ++ "\n" )$
  lift4 (updateSoundSprite hSoundSprite)
	 vol pan pitch


-}
-- Display

primitive garbageCollect "primGC" :: IO ()

disp :: (User -> ImageB) -> IO ()

-- Note: partway into modeling GC's effect on timing.

disp imF =
 do -- garbageCollect
    t0 <- currentSpriteTime
    --putStrLn ("doing spritify for time " ++ show t0)
    (user', userChan) <- newUser t0
    -- Do validity checking.  (For debugging.)
    let user = validateE user'
    tBeforeGCVar <- newRef t0
    -- Initialize SpriteLib.  I wish this could be done in SpriteLib's
    -- DllMain, but DSound initialization bombs there.  Doing it here is
    -- shaky, as it relies on the DDSurface and DSBuffer evaluations being
    -- postponed due to laziness.
    openSpriteLib
    (pChain0, chain) <- spritify (imF user) t0
    --putStrLn "did spritify"
    tPrevVar <- newRef t0
    chainVar <- newRef pChain0
    showSpriteTree
      chain
      (do tNow <- currentSpriteTime
	  tBeforeGC <- getRef tBeforeGCVar
          --putStrLn ("update: time " ++ show tNow)
          tPrev <- getRef tPrevVar
          addUserUpdate userChan (tNow - tPrev) tNow
          updateRefIO chainVar (doUpdateTrees tNow (tNow + updatePeriodGoal))
          -- Add the update event for tSample.  This will stop the
          -- event search, which is only interested in events
          -- *before* tNow.
          afterUpdate <- currentSpriteTime
          --putStr (show (tNow - tPrev) ++ ", ")
{-
          putStrLn ("tNow == " ++ show tNow ++
                    ".  tNow - tPrev == " ++ show (tNow - tPrev) ++
                    ".  tNow - tNowExpected == " ++
                    show (tNow - (tPrev + updatePeriodGoal)) ++
                    ".  Update took " ++ show (afterUpdate - tNow) )
-}
          setRef tPrevVar tNow
      )
      userChan
      t0



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
  -- soundImage (bufferSound bounceBuffer) `over`
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

donut6 u = donut (vector2Polar dist ang) sc
		 (20 * dt)
	      `untilB` userTimeIs delay u -=> emptyImage
  where dt   = 3 * userTime u
	ang  = dt
	dist = 1 - 0.05 * dt
	sc   = dist
	delay = 7

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
   stretch scale $
   textImage text angle) `over`
 donut7 u
  where
   dt = userTime u
   scale = 1.5 + sin dt
   angle = dt / 3
   color = colorHSL h 0.5 0.5
   h = 180 * (1 + sin dt)
   text = simpleText (showB dt)

wordy3 angle u =
 textImage (simpleText (constantB "A String to render")) angle

wordy4 u = wordy3 (sin (userTime u) * pi / 2) u

wordy5 u =
  stretch 2 $
  withColor blue  (wordy3 (  sin (userTime u) * pi / 2) u) `over`
  withColor green (wordy3 (- sin (userTime u) * pi / 2) u)

{-
lotsODonuts u = more `over` donut6 u
 where
   more = emptyImage `untilB` next ==> lotsODonuts
   next = uNext (userTimeIs 1.2) u
-}

lotsODonuts u = accumB over emptyImage another
 where
  another = (alarmE (startTime u) 1.2 `afterE_` u) ==> donut6

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

-- utility:

uNext :: (User -> Event ()) -> User -> Event User

uNext f u = f u `afterE_` u

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
  (x, y) = pairBSplit (vector2XYCoords (mouse u .-. point2XY (-1) (-1.5)))
  -- (x, y) = (1, 1)

snd4 u = accum u `untilB` uNext (keyPress Win32.vK_ESCAPE) u ==> snd4
 where
  accum u = emptyImage `untilB` uNext (keyPress Win32.vK_SPACE) u ==> \ u' ->
            accum u' `over` donut1 u'

snd5 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` uNext (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = silence `untilB` uNext (keyPress Win32.vK_SPACE) u ==> \ u' ->
            bufferSound bounceBuffer `mix` accum u'

snd6 u = soundImage (loop u)
 where
  loop  u = accum u `untilB` uNext (keyPress Win32.vK_ESCAPE) u ==> loop
  accum u = accumB mix silence (addSound u)
  addSound u = withTimeE (bounceButton u)  ==> bounce
  bounceButton u = uNext (keyPress Win32.vK_SPACE) u
  bounce (u',te) = trace ("bounce at " ++ show (startTime u', te) ++ "\n") $
                   bufferSound bounceBuffer


--- 3D !!

-- Import an X mesh file to make a geometry.  Currently presumes that the
-- X file is a simple mesh.
importX :: String -> GeometryB
importX = meshG . meshBuilder

defaultCamera = translate3 (vector3XYZ 0 0 (-4))

teapot, sphere :: GeometryB
-- teapot = meshGeometry (meshBuilder "../Media/tpot2.x")
teapot = rotate3 xVector3 (-pi/2) **% importX "tpot2.x"
-- teapot = uscale3 0.005 **% importX "1701d.x"
--teapot = meshG (meshBuilder "tpot2.x")
sphere = importX "sphere1.x"

g0 u = renderGeometry g defaultCamera
 where
   g = withColorG red $
       rotate3 yVector3 (userTime u) **% sphere

g1 = withSpin potSpin1
g2 = withSpin potSpin2
g3 = withSpin potSpin3

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
  potAngle = integral potAngleSpeed u
  axis = yVector3
  -- axis = rotate3 zVector3 (userTime u) **% yVector3


potSpin3 potAngleSpeed u =
  translate3 (vector3Spherical 0.5 (- userTime u) 0) **%
    potSpin2 potAngleSpeed u

-- Simple moving image of rendered constant geometry
g4 u = move (vector2Polar 0.5 (- userTime u)) $
       renderGeometry (withColorG red teapot) defaultCamera

-- Simpler.  Trying to fix missing light problem.
g5 u = renderGeometry (withColorG red teapot) defaultCamera

{-
gAnims u = loop all u
 where
  all = [g1,g2]

  loop []    u = loop all u
  loop (h:t) u = h u `untilB` 
-}

-- for comparison
h1 u = stretch (growExp u 1) horses


growHowTo :: User -> ImageB

growHowTo u = moveXY 0 (- winHeight / 2 + 0.1) $
              withColor yellow $
              stringBIm messageB
  where
    winHeight = sndB (vector2XYCoords (viewSize u))
    messageB = selectLeftRight "Use mouse buttons to control pot's spin"
               "left" "right" u


grow, growExp :: User -> RealVal -> RealB

grow u x0 = size
 where 
  size = constantB x0 + integral rate u 
  rate = bSign u

-- Yipes!! This one is blows the stack :-(

growExp u x0 = size
 where 
  size = constantB x0 + integral rate u 
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
     uNext lbp u ==> pressed left  lbr .|. 
     uNext rbp u ==> pressed right rbr 

  pressed x stop u = 
   constantB x `untilB` uNext stop u ==> notPressed

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

wildcat = flipImage wildcatFlipBook 0
 where
  wildcatFlipBook = flipBook wildcatSurface w h 0 0 1 1
  wildcatSurface = bitmapDDSurface "../Media/wildcat.bmp"
  (w,h) = getDDSurfaceSize wildcatSurface

horses = flipImage horsesFlipBook 0
 where
  horsesFlipBook = flipBook horsesSurface w h 0 0 1 1
  horsesSurface = bitmapDDSurface "../Media/horses.bmp"
  (w,h) = getDDSurfaceSize horsesSurface


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
  x = userTime u `untilB` userTimeIs 1 u `snapshot` x -=> 1

iTst7 u = withColor red (stretch x (donut0 u))
 where
  x = integral dx u `untilB` userTimeIs 5 u `snapshot` x -=> 1
  dx = 0.2 :: Behavior RealVal

iTst8 u = withColor red (stretch x (donut0 u))
 where
  x = integral dx u `untilB` userTimeIs 10 u `snapshot` x ==> constantB . snd
  dx = 0.1 :: Behavior RealVal

iTst9 u = withColor red (stretch x (donut0 u))
 where
  x = integral dx u `untilB` predicate (x>=*1) u `snapshot` x -=> 1
  dx = 0.1 :: Behavior RealVal

iTst10 u = withColor red (stretch x (donut0 u))
 where
  x = integral dx u `untilB` predicate (x>=*1) u `snapshot` x ==> constantB . snd
  dx = 0.1 :: Behavior RealVal


uPeriod u = showBIm (updatePeriod u)
