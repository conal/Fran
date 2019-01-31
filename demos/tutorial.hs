-- Examples used in the Fran tutorial, which may be found at:
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


patOrbitsCharlotte = bigger wiggle charlotte       `over`
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

orbitAndSlower = orbit `over` slower 2 orbit
  where
   orbit = moveXY wiggle waggle jake

followMouseAndDelay u = follow `over` later 1 follow
  where
   follow = move (mouseMotion u) jake

-- This one uses a delayed motion.  The quality is much better.
followMouseAndDelay' u = follow `over` follow'
  where
   follow  = move (mouseMotion u) jake
   follow' = move (later 1 (mouseMotion u)) jake



delayAnims dt anims =
  overs (zipWith (later . constantB) [0, dt ..] anims)

-- This one will be better when background transparency works

kids u =
  delayAnims 0.5 (map (move (mouseMotion u))
                      [jake, becky, charlotte, pat])

trailWords motion str =
  delayAnims 1 (map moveWord (words str))
  where
    moveWord word = move motion (
                     bigger 2 (
                       withColor blue (stringIm word) ))

flows u = trailWords motion "Time flows like a river"
  where
    motion = 0.7 *^ vector2XY (cos (userTime u)) (sin (2 * (userTime u)))

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
    c1 `untilB` nextUser lbp u ==> cycle c2 c1


tricycle u =
   buttonMonitor u `over`
   withColor (cycle3 green yellow red u) (
     bigger (wiggleRange 0.5 1)
       circle )
  where
   cycle3 c1 c2 c3 u =
    c1 `untilB` nextUser lbp u ==> cycle3 c2 c3 c1


jumpFlower u = buttonMonitor u `over`
               moveXY (bSign u) 0 flower

flower = smaller 2.5 (importBitmap "../Media/rose medium.bmp")

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

buttonMonitor u =
  moveXY 0 (-1) (
   withColor white (
    bigger 2 (
    stringBIm (selectLeftRight "(press a button)" "left" "right" u))))
 

growFlower u = buttonMonitor u `over`
               grow flower u

grow im u = bigger size im 
 where 
  size = 1 + atRate rate u 
  rate = bSign u 


growFlowerExp u = buttonMonitor u `over`
                  grow' flower u

grow' im u = bigger size im 
 where 
  size = 1 + atRate rate u 
  rate = bSign u * size 


-- A fun alternative to "flower" in the previous examples:
godzilla = moveXY 0 (-0.4) (
           smaller 2.5 (importBitmap "../Media/godzilla medium.bmp") )



-- --- Misc to go elsewhere ---

display imB = disp (\ u -> later (constantB (userStartTime u)) imB)

displayU = disp

-- Packing it all up

titles = [ "leftRightCharlotte", "upDownPat", "charlottePatDance",
          "charlottePatDoubleDance", "dance1", "dance2",
          "patOrbitsCharlotte",
          "velBecky", "accelBecky", "mouseVelBecky", "beckyChaseMouse",
          "danceChase",
          "springDragBecky", "orbitAndLater", "orbitAndSlower",
          "followMouseAndDelay", "kids", "flows", "flows2",
          "redBlue", "redBlueCycle", "tricycle",
          "jumpFlower", "growFlower", "growFlowerExp",
          "The End" ]

anims = map const [ talkTitle,
                    leftRightCharlotte, upDownPat, charlottePatDance,
                    charlottePatDoubleDance, dance1, dance2,
                    patOrbitsCharlotte ]
        ++
        [ velBecky, accelBecky, mouseVelBecky, beckyChaseMouse, danceChase,
          springDragBecky, const orbitAndLater, const orbitAndSlower,
          followMouseAndDelay, kids, flows, flows2,
          redBlue, redBlueCycle, tricycle,
          jumpFlower, growFlower, growFlowerExp,
          const emptyI ]

{-
animsAndTitles =
 concat [ [titleIm title, anim]
         | (title,anim) <- zip titles anims]
  where
   titles = [ "leftRightCharlotte", "upDownPat", "charlottePatDance",
              "charlottePatDoubleDance", "dance1", "dance2",
              "patOrbitsCharlotte",
              "velBecky", "accelBecky", "mouseVelBecky", "beckyChaseMouse",
              "danceChase",
              "springDragBecky", "orbitAndLater", "orbitAndSlower",
              "followMouseAndDelay", "flows", "flows2",
              "redBlue", "redBlueCycle", "tricycle",
              "jumpFlower", "growFlower", "growFlowerExp",
              "The End" ]

   titleIm name u =  bigger (25 / fromInt (length name)) (
                        withColor red (stringIm name) )
-}

animsAndTitles = interleave anims animTitles
 where

  animTitles =
   [  later' (tAnim (tsize str (withColor col (stringIm str))))
    | (col,str,tAnim) <- zip3 colors titles
                              (concat (repeat titleAnimators)) ]

  colors = [ colorHSL (constantB h + 30*wiggle) 0.5 0.5 | h <- [0, sep .. 359] ]

  sep = fromInt 360 / fromInt (length titles)

  tsize str = bigger (25 / fromInt (length str))

  interleave (x:xs') ys = x : interleave ys xs'
  interleave _       _  = []

  later' i u = later (constantB (userStartTime u)) i

allAnims u = allRec 0 u
 where
  allRec i u =
     {-whiteBack-} (animsAndTitles!!i) u `untilB`
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

  -- Use a white background
  whiteBack imF u = imF u `over` bigger 3 (withColor white square)

charToVKey :: Char -> VKey
charToVKey = fromInt . fromEnum

talkTitle =
 bigger 1.5 $
 trail 1.2 motion
   [ withColor (colorHSL (constantB h + wiggleColor) 0.5 0.5) (
       bigger 1.5 (stringIm str))
   | (str,h) <- zip titleWords
                    [0, 360 / fromIntegral (length titleWords) .. 359]  ]
 where
  motion = vector2XY (slower 5 (0.3 * waggle))
                     (slower 4 (0.3 * wiggle))
  wiggleColor = slower 2 (180 * wiggle)
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
