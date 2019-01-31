-- Examples used in the Fran tutorial, which may be found at:
-- 
--   http://www.research.microsoft.com/~conal/fran/tutorial.htm
-- 
-- Instructions:
-- 
--   After installing Hugs, double-click on the file Tutorial.hs in the
--   subdirectory lib\Fran\demos. The examples will begin running
--   automatically. Press space, "n", or right-arrow to advance to the
--   next animation, and "p" or left-arrow to back up to the previous
--   one. If you want to display just a single animation, e.g.,
--   leftRightCharlotte, then close the animation window and type "display
--   leftRightCharlotte" to the prompt. You can alter the definition in an
--   editor, save the result, type ":r" to the Hugs prompt, and "$$" again
--   to display the new version. For examples involving user interaction
--   or rate-based animation, use displayU instead of display, as
--   mentioned below.  For 3D examples, use displayG if there no user
--   argument, and displayGU if there is one.

-- Due to bit rot, running this tutorial via "main" often dies after a
-- while with "Program error: IOExts.interleaveIO: thread exited with no
-- result".  You can, however run the examples individually as explained
-- in the header comment.

-- Sadly and mysteriously, 3D performance and visual quality has become
-- terrible.  I'm guessing it's due to changes to Direct3D, but I really
-- don't know.

module Tutorial where

import Fran
import Win32Key
import qualified StaticTypes as S

-- Controls white vs black background, etc.  Use True with pasteUp in
-- ReplayTut.hs.  I couldn't find a satisfactory way to set the background
-- color here, because display, displayU, etc are defined elsewhere.
forArticle = False

(textColor, importKid)
   | forArticle = (black, importKidHiRes)
   | otherwise  = (white, importKidHiRes)   -- what should it be?

main = displayU allAnims

importKidHiRes name =
  -- The small ones came from shrinking the big ones by 35% (without
  -- smoothing, which messes up color-keying).
  stretch 0.35 (
   importBitmap ("../Media/" ++ name ++ "Big.bmp"))

importKidLoRes name =
  importBitmap ("../Media/" ++ name ++ "Small.bmp")

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
   aSmall = stretch 0.5 charlottePatDance2


dance1 = stretch (abs wiggle) charlottePatDance


dance2 = hvDance (stretch wiggle charlotte)
                 (stretch waggle pat)


patOrbitsCharlotte =
  stretch wiggle charlotte   `over`
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
   offset = atRate vel u
   vel    = mouseMotion u

beckyChaseMouse u = move offset becky
  where
   offset = atRate vel u
   vel    = mouseMotion u - offset

chaseMouse im u = move offset im
  where
   offset = atRate vel u
   vel    = mouseMotion u - offset

danceChase u =
  chaseMouse (stretch 0.5 charlottePatDance) u

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
--jake = importBitmap "e:/conal/Pictures/jakeC.bmp"

orbitAndFaster =
 orbit `over` faster 2 orbit
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
  overs (zipWith later [0, dt ..] anims)

kids u =
  delayAnims 0.5 (map (move (mouseMotion u) . stretch 0.7)
                      [jake, becky, charlotte, pat] )

trailWords motion str =
  delayAnims 1 (map moveWord (words str))
  where
    moveWord word = move motion (
                     stretch 2 (
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
     stretch (wiggleRange 0.5 1)
       circle )
  where
   cycle3 c1 c2 c3 u =
    c1 `untilB` nextUser_ lbp u ==> cycle3 c2 c3 c1


jumpFlower u = buttonMonitor u `over`
               moveXY (bSign u) 0 flower

flower = stretch 0.4 (importBitmap "../Media/rose medium.bmp")

bSign :: User -> RealB

bSign = selectLeftRight 0 (-1) 1

selectLeftRight :: a -> a -> a -> User -> Behavior a
selectLeftRight none left right u =
  condB (leftButton  u) (constantB left ) (
    condB (rightButton u) (constantB right) (
      constantB none ))

{-
-- Old, nearly equivalent definition.
selectLeftRight none left right u = notPressed u
 where
  notPressed u =
   constantB none `untilB`
     nextUser_ lbp u ==> pressed left  lbr .|.
     nextUser_ rbp u ==> pressed right rbr

  pressed x stop u =
   constantB x `untilB` nextUser_ stop u ==> notPressed
-}
 
buttonMonitor u =
  moveXY 0 (- height / 2 + 0.25) (
   withColor textColor (
    stretch 2 (
    stringBIm (selectLeftRight "(press a button)" "left" "right" u))))
 where
   (width,height) = vector2XYCoords (viewSize u)


growFlower u = buttonMonitor u `over`
               stretch (grow 1 u) flower

grow size0 u = size
 where
  size = size0 + atRate rate u
  rate = bSign u


growFlowerExp u = buttonMonitor u `over`
                  stretch (growExp 1 u) flower

growExp size0 u = size
 where
  size = size0 + atRate rate u
  rate = bSign u * size


-- A fun alternative to "flower" in the previous examples:
godzilla = moveXY 0 (-0.4) (
           stretch 0.4 (importBitmap "../Media/godzilla medium.bmp") )


-- 3D stuff

sphere :: GeometryB
sphere = stretch3 0.8 (
          importX "../Media/sphere2.x")

teapot :: GeometryB
teapot = uscale3 1.5 **%
         importX "../Media/tpot2.x"

redSpinningPot () =
  turn3 zVector3 (time * pi) (
   withColorG red teapot)

mouseTurn :: GeometryB -> User -> GeometryB
mouseTurn g u =
  turn3 xVector3 y (
   turn3 zVector3 (-x) g)
 where
   (x,y) = vector2XYCoords (pi *^ mouseMotion u)

mouseSpinningPot :: User -> GeometryB
mouseSpinningPot u =
  mouseTurn (withColorG green teapot) u


-- Take (animated) color and angle and yields a colored, turning teapot
spinPot :: ColorB -> RealB -> GeometryB
spinPot potColor potAngle =
  turn3 zVector3 potAngle (
   withColorG potColor teapot)

-- Preliminary version.
spin1' u = buttonMonitor u `over`
           renderGeometry (spinPot red (grow 0 u))
                          defaultCamera

withSpinner :: (RealB -> User -> GeometryB) -> User -> ImageB
withSpinner f u = buttonMonitor u `over`
                  renderGeometry (f (pi * grow 0 u) u) defaultCamera

spin1, spin2 :: User -> ImageB

spin1 = withSpinner spinner1
 where
   spinner1 angle u = spinPot red angle

spin2 = withSpinner spinner2
 where
   spinner2 potAngleSpeed u =
     spinPot (colorHSL time 0.5 0.5)
             (atRate potAngleSpeed u)


sphereLowRes = importX "../Media/sphere0.x"

-- PROBLEM: If movingLight, potAndLight, etc, below are CAFs, they'll
-- space-leak like crazy!  This problem can be fixed by providing GC
-- support for memoization.  For now, make them not be CAFs, which is
-- unsatisfying as well.

movingLight' () =
   move3 motion (
    stretch3 0.1 (
     withColorG yellow (
      sphereLowRes `unionG` pointLightG)))
 where
  motion = vector3Spherical 1.5 (pi*time) (2*pi*time)

movingLight () =
  translate3 motion **%
  uscale3 0.1       **%
  withColorG yellow (
    sphereLowRes `unionG` pointLightG)
 where
  motion = vector3Spherical 1.5
             (pi*time) (2*pi*time)

potAndLight () = withColorG green teapot `unionG` movingLight ()

delayAnims3 dt anims =
  unionGs (zipWith later [0, dt ..] anims)
  -- unionGs [ later delay anim
  --         | (delay,anim) <- zip [0, dt ..] anims ]

potAndLights () =
  slower 5 (
   withColorG green teapot `unionG`
   -- redSpinningPot () `unionG`
   delayAnims3 (2/5) (replicate 5 (movingLight ())))

spiral3D () = delayAnims3 0.075 balls
 where
   ball   = move3 motion (stretch3 0.1 sphereLowRes)
   balls  = [ withColorG (bColor i) ball | i <- [1 .. n] ]
   motion = vector3Spherical 1.5 (10*time) time
   
   n      = 20
   bColor i =
     colorHSL (2 * pi * fromInt i / fromInt n) 0.5 0.5

spiralTurn () = turn3 zVector3 (pi*time) (unionGs (map ball [1 .. n]))
 where
   n = 40
   ball i  = withColorG color (move3 motion (stretch3 0.1 sphereLowRes))
    where
      motion = vector3Spherical 1.5 (10*phi) phi
      phi    = pi * fromInt i / fromInt n
      color  = colorHSL (2*phi) 0.5 0.5

tst0 () = (constantB (putStrLn "hey!!") `ats` [0]) !! 5

-- time-leaks a lot with hue=2*phi, but not with hue=0
tst1 () = turn3 zVector3 (pi*time) (unionGs $ map ball [1 .. n])
 where
   n = 40
   ball i  = withColorG color (move3 motion (stretch3 0.1 sphereLowRes))
    where
      motion = vector3Spherical 1.5 (10*phi) phi
      phi    = pi * fromInt i / fromInt n
      color  = colorHSL hue 0.5 0.5
      hue    = 2*phi

-- Make some static stuff explicit.
tst2 () = turn3 zVector3 (pi*time) (unionGs (map ball [1 .. n]))
 where
   n = 40
   ball i  = withColorG (constantB color) $
             move3 (constantB motion)     $
             stretch3 0.1 sphereLowRes
    where
      motion = S.vector3Spherical 1.5 (10*phi) phi
      phi    = pi * fromInt i / fromInt n
      color  = S.colorHSL hue 0.5 0.5
      hue    = 2*phi


-- 2D variant
tst2D n = turn (pi*time/3) (overs $ map ball [1 .. n])
 where
   ball i  = withColor color $ move motion $ stretch 0.05 circle
    where
      motion = vector2Polar 1 theta
      theta  = 2 * pi * fromInt i / fromInt n
      color  = colorHSL hue 0.5 0.5
      hue    = theta


-- Packing it all up

fmd u = later 1 (move (mouseMotion u) jake)

fmd' u = move (later 1 (mouseMotion u)) jake

{-
delayAnims dt anims =
  overs (zipWith later [0, dt ..] anims)

kkids u =
  delayAnims 0.5 (map (move (mouseMotion u) . stretch 0.7)
                      [jake, becky] )
-}

titles = [ "leftRightCharlotte", "upDownPat", "charlottePatDance"
         , "charlottePatDoubleDance", "dance1", "dance2"
         , "patOrbitsCharlotte"
         , "velBecky", "accelBecky", "mouseVelBecky", "beckyChaseMouse"
         , "danceChase"
         , "springDragBecky", "orbitAndLater", "orbitAndFaster"
         , "followMouseAndDelay", "kids", "flows", "flows2"
         , "redBlue", "redBlueCycle", "tricycle"
         , "jumpFlower", "growFlower", "growFlowerExp"
         , "sphere", "teapot", "redSpinningPot", "mouseSpinningPot"
         , "spin1", "spin2", "potAndLight", "potAndLights"
         , "spiral3D", "spiralTurn"
         , "The End"
         ]

anims = [ talkTitle ] ++
        map const [ leftRightCharlotte, upDownPat, charlottePatDance
                  , charlottePatDoubleDance, dance1, dance2
                  , patOrbitsCharlotte
                  ]
        ++
        [ velBecky, accelBecky, mouseVelBecky, beckyChaseMouse, danceChase
        , springDragBecky, const orbitAndLater, const orbitAndFaster
        , followMouseAndDelay, kids, flows, flows2
        , redBlue, redBlueCycle, tricycle
        , jumpFlower, growFlower, growFlowerExp
        ]
        ++
        map (\ gf -> \ u -> renderGeometry (gf u) defaultCamera )
            [ const sphere, const teapot, const (redSpinningPot ())
              , mouseSpinningPot ]
        ++
        [ spin1, spin2 ]
        ++
        map (\ g -> \ u -> renderGeometry g defaultCamera )
            [ potAndLight (), potAndLights ()
            , spiral3D (), spiralTurn () ]


-- Interleave the animations and titles.  Note that there's an extra
-- animation at the start and an extra title at the end.

animsAndTitles = interleave anims animTitles
 where

  animTitles =
   [  \ u -> later' (tAnim (tsize str (withColor col (stringIm str)))) u
    | (col,str,tAnim) <- zip3 colors titles
                              (concat (repeat titleAnimators)) ]

  colors = [ colorHSL (constantB h + wiggle/2) 0.5 0.5
           | h <- [0, sep .. 2*pi] ]

  sep = 2*pi / fromInt (length titles)

  tsize str = stretch (20 / fromInt (length str))

  interleave (x:xs') ys = x : interleave ys xs'
  interleave _       _  = []

  later' i u = later (constantB (userStartTime u)) i


-- Step through some User->ImageB functions using keys
stepThrough :: [User -> ImageB] -> (User -> ImageB)

stepThrough imFs u = allRec 0 u
 where
  allRec i u =
     (imFs!!i) u `untilB`
       newIndex u `afterE` u ==> uncurry allRec
    where
      newIndex u = --plusMinus u `suchThat` validIndex
                   plusMinus u ==> (`mod` lenAnims)
      validIndex i' = 0 <= i' && i' < lenAnims
      plusMinus u = keyIn nextKeys u -=> i+1  .|.
                    keyIn prevKeys u -=> i-1
      keyIn chars u =
        keyPressAny u `suchThat` (`elem` chars)

  lenAnims = length imFs
  nextKeys = [vK_SPACE,vK_RIGHT, charToVKey 'N']
  prevKeys = [vK_LEFT, charToVKey 'P']

allAnims = stepThrough animsAndTitles

charToVKey :: Char -> VKey
charToVKey = fromInt . fromEnum

talkTitle u =
 stretch 1.5 $
 trail 1.2 motion
   [ withColor (colorHSL (constantB h + wiggleColor) 0.5 0.5) $
     stretch 1.1 $
     stringIm str
   | (str,h) <- zip titleWords [0, sep .. 2*pi]  ]
 where
  motion = vector2XY (slower 5 (0.3 * waggle))
                     (slower 4 (0.3 * wiggle))
  wiggleColor = slower 2 (pi * wiggle)
  sep         = 2 * pi / fromInt (length titleWords)
  titleWords = words "Functional Reactive Animation"

  trail dt motion anims =
    delayAnims dt (map (move motion) anims)

-- Now let's look at some simple title animators

rockT,growT,orbitT :: ImageB -> ImageB

-- First, a rocking motion, in which we move down, rotate back and forth
-- slightly, and move back up.

rockT = moveXY 0 0.1        .
        turn (wiggle / 10)  .
        moveXY 0 (-0.2)

-- Then a pulsating scaling:

growT = stretch (wiggleRange 0.7 1.3)

-- And finally a small circular motion:

orbitT = moveXY (0.3*wiggle) (0.3*waggle)

-- To compose the title animators we double up the simple ones plus the
-- identity animator.

titleAnimators = [ a . a' | a  <- simples, a'  <- simples ]
 where
  simples = [id, rockT, growT, orbitT]
