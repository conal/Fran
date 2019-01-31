-- Examples for OCATE talk
--
-- Last modified Thu Oct 31 14:18:01 1996
--
-- Directions: do "disp allAnims".  Press space, "n", or right-arrow to
-- advance to the next animation, and "p" or left-arrow to back up to
-- previous one.
-- 
-- To do:
--  + Find bigger bitmaps for examples

module Samples where

import RBMH
import UtilsB
import qualified StaticTypes as S
import ImageBTest (disp, seqImF)

anims :: [Time -> ImageB]

anims = [anim0,
	 l anim1, l anim2, l anim3, l anim4, l anim5, anim6, anim7, anim8,
	 anim9, anim10, anim11, anim12, anim13, anim14, anim15, anim16, 
         l questions]
 where 
   l = later'

later' = flip later

titles = [
  "   ",
  "Example",
  "Composition",
  "More variations, A",
  "More variations, B",
  "More variations, C",
  "Rate-based animation",
  "Spring with drag",
  "Fun with time shifting",
  "More fun with time shifting",
  "Another one",
  "A reactive behavior",
  "Cyclic reactivity",
  "More action",
  "Choice",
  "Choice-based growth",
  "Exponential growth variation",
  "? ? ?"]

-- Composing Animations in Haskell

anim0 t0 =
 bigger 2 $
 tracker 1.2 motion
   [ withColor (hsl (lift0 h + wiggleColor) 0.5 0.5) (
       stringIm str)
   | (str,h) <- zip ["Composing", "Animations",
		     "in", "Haskell"]
		    [0, 360 / 4 .. 359]  ]
 where
  motion = point2XY (slower 2 (0.9*wiggle/3))
		    (slower 5 (1.5*wiggle'/5))
  wiggleColor = slower 2 (90 * wiggle)

-- Example

anim1' =
  moveXY wiggle 0  doll  `over`
  moveXY 0 wiggle' dude

doll = bigger 2 $ importBitmap "../Media/doll.bmp"
dude = bigger 2 $ importBitmap "../Media/dude.bmp"

-- Composition

hvDance im1 im2 =
  moveXY wiggle 0  im1  `over`
  moveXY 0 wiggle' im2
                
anim1 = hvDance doll dude

anim2 = hvDance aSmall aSmall
 where
  aSmall = smaller 2 anim1

-- More variations

anim3 = bigger (abs wiggle) anim1

anim4 = hvDance (bigger wiggle  doll)
                (bigger wiggle' dude)

anim5 = bigger wiggle doll       `over`
        moveXY wiggle wiggle' dude

-- Rate-based animation

anim6 t0 = move pos dude
 where
  pos  = origin2 .+^ atRate rate t0
  rate = mouse t0 .-. pos

-- Spring with drag

anim7 t0 = move pos doll
 where
  pos   = origin2 .+^ atRate rate t0
  rate  = atRate accel t0
  accel = (mouse t0 .-. pos) - rate ^/ 2


-- Fun with time shifting

anim8 t0 = marker p2 red  `over`
           marker p3 blue `over`  curve
 where
  curve = bezier p1 p2 p3 p4
  p1    = point2XY (-1) 0
  p2    = mouse t0
  p3    = later 2 p2
  p4    = point2XY 1 0
  marker p col =
    move p (smaller 20
             (withColor col circle))

-- More fun with time shifting

tracker :: Time -> Point2B -> [ImageB] -> ImageB

tracker spc motion trail =
 foldr1 over
  [ later dt (move motion im) |
    (dt,im) <- zip [0, spc ..] trail ]

anim9 t0 =
 withColor yellow (
  tracker 0.5 (mouse t0)
   [bigger 1.5 (stringIm str)
    | str <- ["Time", "flows", "like", "a", "river"]] )

pat t0 =
 withColor yellow (
  tracker 0.5 (mouse t0)
   [stringIm str | str <- ["Pat", "is", "a", "funny", "little", "chap"] ] )

-- One more

anim10 t0 =
 tracker 1 (mouse t0)
   [smaller 10 (
     withColor (hsl (lift0 h) 0.5 0.5)
       circle )
    | h <- [0, 360 / 5 .. 359] ]


-- A reactive behavior

anim11 t0 = withColor redBlue circle
 where
  redBlue = red `untilB` lbp t0 -=> blue

-- Cyclic reactivity

anim12 t0 =
  withColor (cycle red blue t0) circle
 where
  cycle c1 c2 t0 =
   c1 `untilB` lbp t0 *=> cycle c2 c1

-- More action

anim13 t0 =
  withColor (cycle3 green yellow red t0) (
    bigger (wiggleRange 0.5 1)
      circle )
 where
  cycle3 c1 c2 c3 t0 =
   c1 `untilB` lbp t0 *=> cycle3 c2 c3 c1

-- Choice

bSign t0 =
  0 `untilB`
    lbp t0 *=> nonZero (-1) lbr  .|.
    rbp t0 *=> nonZero   1  rbr
 where
  nonZero r stop t1 =
   r `untilB` stop t1 *=> bSign

anim14 t0 = moveXY (bSign t0) 0 flower

flower = importBitmap "../Media/flwrblu.bmp"

-- Choice-based growth

grow im t0 = bigger size im
 where
  size = 1 + atRate rate t0
  rate = bSign t0

anim15 = grow flower

-- Exponential growth variation

grow' im t0 = bigger size im
 where
   size = 1 + atRate rate t0
   rate = bSign t0 * size

anim16 = grow' flower

-- Questions?

questions =
  smaller (time/3) (
   turnRight time (
    moveXY 0 0.5 (
     bigger 2 (
      withColor red (stringIm "Questions?") ) ) ) )

{- Or:

questions =
  smaller (time/3)   $
  turnRight (time*2) $
  moveXY 0 1         $
  bigger 3           $
  withColor red      $
  stringIm "Questions?"

-}

{-

growShrink im t0 = bigger (size 1 t0) im
 where
   size s0 t0 =
     s0 `untilB` lbp t0 *=> size (s0/2)
             .|. rbp t0 *=> size (s0*2)

anim13 = growShrink flower

smoothGrowShrink im t0 =
 bigger (exp (atRate (rate t0) t0)) im
 where
   rate t0 =
       0 `untilB`
         lbp t0 *=> changing (-1) lbr
     .|. rbp t0 *=> changing   1  rbr
   changing r stop t1 =
     r `untilB` stop t1 *=> rate
-}

{-
smoothGrowShrink' im t0 = bigger size im
 where
   size = atRate rate t0
   rate = grow t0 * size
   grow t0 =
       0 `untilB`
         lbp t0 *=> changing (-1) lbr
     .|. rbp t0 *=> changing   1  rbr
   changing r stop t1 =
     r `untilB` stop t1 *=> grow

anim15 = smoothGrowShrink' flower

-- Unused:

-- Working events harder

growShrink' im t0 = bigger (size t0 1) im
 where
   size t0 s0 =
     s0 `untilB` (lbp t0 -=> (s0/2)
              .|. rbp t0 -=> (s0*2))
     +=> size

-- Choice with growth

smoothGrowShrink0 im t0 =
  bigger (1 + atRate (changingRate t0) t0) im
  where
    changingRate t0 =
      0 `untilB` lbp t0 *=> growing    .|.
                 rbp t0 *=> shrinking
    growing   t1 = -1 `untilB` lbr t1 *=> changingRate
    shrinking t1 =  1 `untilB` rbr t1 *=> changingRate

-- Tweak growth rate

smoothGrowShrink1 im t0 =
  bigger (exp (atRate (changingRate t0) t0)) im
  where
    changingRate t0 =
      0 `untilB` lbp t0 *=> growing    .|.
                 rbp t0 *=> shrinking
    growing   t1 = -1 `untilB` lbr t1 *=> changingRate
    shrinking t1 =  1 `untilB` rbr t1 *=> changingRate

-- Factoring

smoothGrowShrink2 im t0 =
  bigger (exp (atRate (changingRate t0) t0)) im
  where
    changingRate t0 =
      0 `untilB` lbp t0 *=> changing (-1) lbr .|.
                 rbp t0 *=> changing   1  rbr
    changing rate stop t1 =
      rate `untilB` stop t1 *=> changingRate

-- More factoring

smoothGrowShrink3 im t0 =
  bigger (exp (atRate (changingRate t0) t0)) im
  where
    changingRate t0 =
      0    `untilB` setRate +=> \ t1 (rate, stop) ->
      rate `untilB` stop t1 *=>
      changingRate
      where
       setRate = lbp t0 -=> (-1,lbr) .|.
                 rbp t0 -=> ( 1,rbr)

smoothGrowShrink4 im t0 =
  bigger (exp (atRate (changingRate t0) t0)) im
  where
    changingRate t0 =
      0    `untilB` setRate t0 ==> \ (rate, stop) ->
      rate `untilB` stop       *=>
      changingRate
    -- Occurs when a button is pressed, yielding the new rate and
    -- a stop event.
    setRate t0 = lbp t0 *=> (\ t1 -> (-1, lbr t1))   .|.
                 rbp t0 *=> (\ t1 -> ( 1, rbr t1))


a0 = smoothGrowShrink0 flower
a1 = smoothGrowShrink1 flower
a2 = smoothGrowShrink2 flower
a3 = smoothGrowShrink3 flower
a4 = smoothGrowShrink4 flower


-}

{-

-- A larger example

orbits =
 guy `over` yOrbit `over` rOrbit `over` back
 where
  guy = withColor royalBlue (
         moveXY 0.5 0 (
          biggerXY wiggle wiggle' xguy ) )
  yOrbit = withColor yellow (
             moveXY w w' (
               bigger (0.1 - 0.08 * wiggle)
                 circle ) )
  rOrbit = withColor red (
             moveXY w' w (
               bigger (0.1 + 0.06 * wiggle')
                 circle ) )
  back = withColor (hsl (wiggle * 180) 1 1) (
           smaller 2 circle )
  w  = 0.6 * wiggle
  w' = 0.6 * wiggle'

xguy = importBitmap "../Media/xguy.bmp"

-}


------- Sequencing and titling stuff


animsAndTitles =
  concat [ [later' (tAnim (tsize str (withColor col (stringIm str)))), anim]
          | (col,str,(anim,tAnim)) <- zip3 colors titles
				       (zip anims
					 (concat (repeat titleAnimators))) ]
 where colors = 
         [(hsl (lift0 h + 30*wiggle) 0.5 0.5)
          | h <- [0, sep .. 359] ]
       sep = fromInt 360 / fromInt (length titles)
       tsize str = bigger (30 / fromInt (length str))


-- Some title animators
simpleTitleAnimators = [id, rockT, growT, orbitT]

-- Double them up
titleAnimators = [ a . a' | a  <- simpleTitleAnimators,
			    a' <- simpleTitleAnimators ]

rockT,growT,orbitT :: ImageB -> ImageB


rockT = moveXY 0 0.1             .
	turnRight (wiggle / 10)	 .
	moveXY 0 (-0.2)

growT = bigger (wiggleRange 0.7 1.3)

orbitT = moveXY (0.3*wiggle) (0.3*wiggle')

-- allAnims = foldr1 seqImF animsAndTitles

{-  Oops!  spatial interaction problem
allSized t0 =
  viewStretch (S.vector2XY 3 3) (allAnims t0) t0
-}

-- This version lets you step back and forth

allAnims t0 = allRec t0 0
 where
  allRec t0 i = (animsAndTitles!!i) t0 `untilB` newIndex +=> allRec
    where
      newIndex = (plusMinus `suchThat` validIndex) t0
      validIndex i' = 0 <= i' && i' < lenAnims
      plusMinus t0 = keyIn " N'" t0 -=> i+1 .|.      -- "<space>n<right>"
		     keyIn "%P"  t0 -=> i-1	     -- "<left>p"
      keyIn chars =
	keyPress `suchThat` (\(ch,_) -> ch `elem` chars)
  lenAnims = length animsAndTitles


{-
-- Move this guy into ImageBTest, factored out of the version there, which
-- gets renamed to viewStretchBmpFile

viewStretch size im t0 =
  scale2 (vector2XY (wWidth / lift0 iWidth)
		    (wHeight / lift0 iHeight))
   *% im
  where
    (wWidth, wHeight) = pairBSplit (vector2XYCoords (viewSize t0))
    (iWidth, iHeight) = S.vector2XYCoords size
-}
