-- Examples used in the Fran tutorial, which may be found at:
--
-- Last modified Tue Oct 14 11:12:36 1997
-- 
--   http://www.research.microsoft.com/~conal/fran/tutorial.hs
-- 
-- Instructions (also found in tutorial.hs):
-- 
--   After installing Hugs from [Hugs 1.4], double-click on the file
--   tutorial.hs in the subdirectory lib\Fran\demos. The examples will begin
--   running automatically. Press space, "n", or right-arrow to advance to
--   the next animation, and "p" or left-arrow to back up to the previous
--   one. If you want to display just a single animation, e.g.,
--   leftRightCharlotte, then close the animation window and type "display
--   leftRightCharlotte" to the prompt. You can alter the definition in an
--   editor, save the result, type ":r" to the Hugs prompt, and "$$" again
--   to display the new version. For examples involving user interaction or
--   rate-based animation, use displayT instead of display, as mentioned
--   below.


module Tutorial where

import Fran     -- Bring RBMH into scope
import Win32Key

--backColor = white; textColor = black
backColor = black; textColor = white

main = displayU allAnims

importKid name =
  importBitmap ("../Media/" ++ name ++ " head black background.bmp")


leftRightCharlotte = moveXY wiggle 0 charlotte

charlotte = importKid "charlotte"


upDownPat = moveXY 0 waggle pat

pat = importKid "pat"


charlottePatDance = leftRightCharlotte `over` upDownPat


hvDance im1 im2 =
  moveXY wiggle 0 im1  `over`
  moveXY 0 waggle im2


charlottePatDance2 = hvDance charlotte pat


charlottePatDoubleDance = hvDance aSmall aSmall
  where
   aSmall = smaller 2 charlottePatDance2


dance1 = bigger (abs wiggle) charlottePatDance


dance2 = hvDance (bigger wiggle charlotte)
                 (bigger waggle pat)


patOrbitsCharlotte =
  bigger wiggle charlotte   `over`
  moveXY wiggle waggle pat

-- Rate-based animation

velBecky u = moveXY x 0 becky
  where
    x = -1 + atRate 1 u

becky = importKid "becky"

accelBecky u = moveXY x 0 becky
  where
    x = -1 + atRate v u
    v =  0 + atRate 1 u

mouseVelBecky u = move offset becky
  where
   offset = atRate (mouseMotion u) u

beckyChaseMouse u = move offset becky
  where
   offset = atRate vel u
   vel    = mouseMotion u - offset

chaseMouse im u = move offset im
  where
   offset = atRate vel u
   vel    = mouseMotion u - offset

danceChase u =
  chaseMouse (smaller 2 charlottePatDance) u

springDragBecky u = move offset becky
  where
   offset = atRate vel u
   vel    = atRate accel u
   accel  = (mouseMotion u - offset) - 0.5 *^ vel

-- Composition in Time

orbitAndLater = orbit `over` later 1 orbit
  where
   orbit = moveXY wiggle waggle jake

jake = importKid "jake"

orbitAndSlower =
 orbit `over` slower 2 orbit
  where
   orbit = moveXY wiggle waggle jake

followMouseAndDelay u =
 follow `over` later 1 follow
  where
   follow = move (mouseMotion u) jake

-- This one uses a delayed motion.  The quality is much better.
followMouseAndDelay' u =
  follow `over` follow'
  where
   follow  = move (mouseMotion u) jake
   follow' = move (later 1 (mouseMotion u)) jake



delayAnims dt anims =
  overs (zipWith (later . constantB) [0, dt ..] anims)

-- This one will be better when background transparency works

kids u =
  delayAnims 0.5 (map (move (mouseMotion u))
                      [ jake
                      , stretch 0.7  becky
                      , stretch 0.65 charlotte
                      , stretch 0.6  pat
                      ])

moreKids u =
  moveXY 0 (- height / 2 + 0.15) (
   withColor textColor (
    bigger 2 (
      stringBIm message)))  `over`
  delayAnims 0.5 (map (move path)
                      (zipWith stretch
                               (map constantB scales) (cycle kidsL)))
 where
  kidsL   = [bigger 1.2 jake, becky, charlotte, pat]
  scales  = [1, (1 - 2 / fromInt numKids) .. -1]
  -- Increase numKids on a faster computer/video card.
  numKids = 15
  path    = size *^ vector2Polar (sin (fromIntB n*time)) time
  n       = stepper 0 (scanlE (+) 0 changeN)
  changeN = lbp u -=> (-1) .|. rbp u -=> 1
  size    = (height `min` width) / 2.7  -- extra to keep heads in window
  message = constantB "right or left click" `untilB` changeN -=> showB n
  (width,height) = vector2XYCoords (viewSize u)

trailWords motion str =
  delayAnims 1 (map moveWord (words str))
  where
    moveWord word = move motion (
                     bigger 2 (
                       withColor blue (stringIm word) ))

flows u = trailWords motion "Time flows like a river"
  where
    motion = 0.7 *^ vector2XY (cos time) (sin (2 * time))

flows2 u = trailWords (mouseMotion u) "Time flows like a river"



-- Reactive Animation

redBlue u = buttonMonitor u `over`
            withColor c circle
  where
   c = red `untilB` lbp u -=> blue


redBlueCycle u = buttonMonitor u `over`
                 withColor (cycle red blue u) circle
  where
   cycle c1 c2 u =
    c1 `untilB` nextUser_ lbp u ==> cycle c2 c1


tricycle u =
   buttonMonitor u `over`
   withColor (cycle3 green yellow red u) (
     bigger (wiggleRange 0.5 1)
       circle )
  where
   cycle3 c1 c2 c3 u =
    c1 `untilB` nextUser_ lbp u ==> cycle3 c2 c3 c1


jumpFlower u = buttonMonitor u `over`
               moveXY (bSign u) 0 flower

flower = smaller 2.5 (importBitmap "../Media/rose medium.bmp")

bSign :: User -> RealB

bSign = selectLeftRight 0 (-1) 1

selectLeftRight :: a -> a -> a -> User -> Behavior a

selectLeftRight none left right u =
  condB (leftButton  u) (constantB left ) (
    condB (rightButton u) (constantB right) (
      constantB none ))
 
buttonMonitor u =
  moveXY 0 (-1) (
   withColor textColor (
    bigger 2 (
    stringBIm (selectLeftRight "(press a button)" "left" "right" u))))


{-
selectLeftRight none left right u = notPressed u
 where
  notPressed u =
   constantB none `untilB`
     nextUser_ lbp u ==> pressed left  lbr .|.
     nextUser_ rbp u ==> pressed right rbr

  pressed x stop u =
   constantB x `untilB` nextUser_ stop u ==> notPressed
-}

growFlower u = buttonMonitor u `over`
               bigger (grow u) flower

grow u = size
 where
  size = 1 + atRate rate u
  rate = bSign u


growFlowerExp u = buttonMonitor u `over`
                  bigger (grow' u) flower

grow' u = size
 where
  size = 1 + atRate rate u
  rate = bSign u * size


-- A fun alternative to "flower" in the previous examples:
godzilla = moveXY 0 (-0.4) (
           smaller 2.5 (importBitmap "../Media/godzilla medium.bmp") )


-- 3D stuff

-- These first two differ by how closely they approximate a sphere.  The
-- second does a better job, but is more expensive.
sphereLowRes = importX "../Media/sphere1.x"
sphere = importX "../Media/sphere.x"
teapot = uscale3 2                **%
         rotate3 xVector3 (-pi/2) **%
         importX "../Media/tpot2.x"

redSpinningPot =
  rotate3 yVector3 time **%
  withColorG red teapot

mouseSpinningPot :: User -> GeometryB
mouseSpinningPot u =
  rotate3 xVector3 y **%
  rotate3 yVector3 x **%
  withColorG green teapot
 where
   (x,y) = vector2XYCoords (pi *^ mouseMotion u)

-- Not used: spin g with the mouse position and render
gRender :: GeometryB -> User -> ImageB
gRender g u = renderGeometry g' defaultCamera
 where
  g' = rotate3 xVector3 y    **%
       rotate3 yVector3 (-x) **%
       g
  (x,y) = vector2XYCoords (pi *^ mouseMotion u)

spinPot :: ColorB -> RealB -> GeometryB
spinPot potColor potAngle =
  rotate3 yVector3 potAngle **%
  withColorG potColor teapot

spin1, spin2 :: User -> ImageB
spin1 = withSpin potSpin1
spin2 = withSpin potSpin2

potSpin1, potSpin2 :: RealB -> User -> GeometryB

potSpin1 angle u = spinPot red angle

potSpin2 potAngleSpeed u =
  spinPot potColor potAngle `unionG` light
 where
  light = rotate3 yVector3 (pi/4)   **%
          translate3 (vector3Spherical
                          2 time 0) **%
          uscale3 0.1               **%
          withColorG white (
           sphereLowRes `unionG` pointLightG)
  potColor =
     colorHSL (slower 10 wiggle * pi) 0.5 0.5
  potAngle = integral potAngleSpeed u


withSpin :: (RealB -> User -> GeometryB) -> User -> ImageB
withSpin f u = buttonMonitor u `over`
               renderGeometry (f (grow u) u) defaultCamera

growHowTo :: User -> ImageB
growHowTo u =
  moveXY 0 (-1) (
   withColor yellow (
    stringBIm messageB ))
 where
   messageB =
     selectLeftRight
       "Use mouse buttons to \
            \control pot's spin"
       "left" "right" u



--- Misc to go elsewhere ---

uDelay :: (GBehavior bv, TimeTransformable bv) => bv -> User -> bv
uDelay bv u = later (constantB (userStartTime u)) bv

display imB = disp (uDelay imB)

displayU = disp

displayG g   = display (renderGeometry g defaultCamera)
displayGU gf = displayU (\ u -> renderGeometry (gf u) defaultCamera)

-- Stand back far enough to look at a unit sphere comfortably
defaultCamera = translate3 (vector3XYZ 0 0 (-5))

-- Packing it all up

titles = [ "leftRightCharlotte", "upDownPat", "charlottePatDance"
         , "charlottePatDoubleDance", "dance1", "dance2"
         , "patOrbitsCharlotte"
         , "velBecky", "accelBecky", "mouseVelBecky", "beckyChaseMouse"
         , "danceChase"
         , "springDragBecky", "orbitAndLater", "orbitAndSlower"
         , "followMouseAndDelay", "kids", "moreKids", "flows", "flows2"
         , "redBlue", "redBlueCycle", "tricycle"
         , "jumpFlower", "growFlower", "growFlowerExp"
         , "sphere", "teapot", "redSpinningPot", "mouseSpinningPot"
         , "spin1", "spin2"
         , "The End"
         ]

anims = map const [ talkTitle
                  , leftRightCharlotte, upDownPat, charlottePatDance
                  , charlottePatDoubleDance, dance1, dance2
                  , patOrbitsCharlotte
                  ]
        ++
        [ velBecky, accelBecky, mouseVelBecky, beckyChaseMouse, danceChase
        , springDragBecky, const orbitAndLater, const orbitAndSlower
        , followMouseAndDelay, kids, moreKids, flows, flows2
        , redBlue, redBlueCycle, tricycle
        , jumpFlower, growFlower, growFlowerExp
        ]
        ++
        map (\ gf -> \ u -> renderGeometry (gf u) defaultCamera )
            [ const sphere, const teapot, const redSpinningPot
              , mouseSpinningPot ]
        ++
        [ spin1, spin2 ]

-- Interleave the animations and titles.  Note that there's an extra
-- animation at the start and an extra title at the end.

animsAndTitles = interleave anims animTitles
 where

  animTitles =
   [  later' (tAnim (tsize str (withColor col (stringIm str))))
    | (col,str,tAnim) <- zip3 colors titles
                              (concat (repeat titleAnimators)) ]

  colors = [ colorHSL (constantB h + wiggle/2) 0.5 0.5
           | h <- [0, sep .. 2*pi] ]

  sep = 2*pi / fromInt (length titles)

  tsize str = bigger (25 / fromInt (length str))

  interleave (x:xs') ys = x : interleave ys xs'
  interleave _       _  = []

  later' i u = later (constantB (userStartTime u)) i

allAnims u = allRec 0 u
 where
  allRec i u =
     withBack (animsAndTitles!!i) u `untilB`
       (newIndex u `afterE` u) ==> uncurry allRec
    where
      newIndex u = plusMinus u `suchThat` validIndex
      validIndex i' = 0 <= i' && i' < lenAnims
      plusMinus u = keyIn nextKeys u -=> i+1  .|.
                    keyIn prevKeys u -=> i-1
      keyIn chars u =
        keyPressAny u `suchThat` (`elem` chars)
  lenAnims = length animsAndTitles
  nextKeys = [vK_SPACE,vK_RIGHT, charToVKey 'N']
  prevKeys = [vK_LEFT, charToVKey 'P']

  -- Add a background
  withBack imF u = imF u `over` withColor backColor solidImage

charToVKey :: Char -> VKey
charToVKey = fromInt . fromEnum

talkTitle =
 bigger 1.5 $
 trail 1.2 motion
   [ withColor (colorHSL (constantB h + wiggleColor) 0.5 0.5) (
       bigger 1.5 (stringIm str))
   | (str,h) <- zip titleWords [0, sep .. 2*pi]  ]
 where
  motion = vector2XY (slower 5 (0.3 * waggle))
                     (slower 4 (0.3 * wiggle))
  wiggleColor = slower 2 (pi * wiggle)
  sep         = 2 * pi / fromIntegral (length titleWords)
  titleWords = words "Functional Reactive Animation"

  trail dt motion anims =
    delayAnims dt (map (move motion) anims)

-- Now let's look at some simple title animators

rockT,growT,orbitT :: ImageB -> ImageB

-- First, a rocking motion, in which we move down, rotate back and forth
-- slightly, and move back up.

rockT = moveXY 0 0.1             .
        turnRight (wiggle / 10)  .
        moveXY 0 (-0.2)

-- Then a pulsating scaling:

growT = bigger (wiggleRange 0.7 1.3)

-- And finally a small circular motion:

orbitT = moveXY (0.3*wiggle) (0.3*waggle)

-- To compose the title animators we double up the simple ones plus the
-- identity animator.

titleAnimators = [ a . a' | a  <- simples, a'  <- simples ]
 where
  simples = [id, rockT, growT, orbitT]
