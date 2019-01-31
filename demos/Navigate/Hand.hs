-- Models a hand that has a position and can grab and release.

module Hand where

import Fran
import Win32Key

-- Motion, grab, release, grabbing
data Hand = Hand Point2B (Event ()) (Event ()) BoolB

mkHand :: Point2B -> Event () -> Event () -> Hand
mkHand pos tryGrab tryRelease = Hand pos grab release grabbing
 where
   -- Does debouncing, which is useful for keypress-based hands.
   grabbing = stepper False (grab -=> True .|. release -=> False)
   grab     = tryGrab `whenE` notB grabbing
   release  = tryRelease `whenE` grabbing

instance Renderable Hand where
 render hand@(Hand pos grab release grabbing) =
   moveTo pos $
   ifB grabbing closedHand openHand

-- Hand images.  Colored dots for now, but should use good-looking bitmaps.
openHand, closedHand :: ImageB
openHand   = colDot red
closedHand = colDot green
colDot col = withColor col $ stretch 0.05 $ circle


-- Some hand builders

mouseHand, keyHand, stylusHand :: User -> Hand

mouseHand u = mkHand (mouse u) (lbp u) (lbr u)

keyHand u = mkHand pos (keyPress vK_SHIFT u) (keyRelease vK_SHIFT u)
 where
   pos = origin2 .+^ atRate vel u
   vel = vector2XY (keyVal vK_LEFT vK_RIGHT u)
                   (keyVal vK_DOWN vK_UP    u)

-- -1 when only neg pressed, 1 when only pos, 0 when neither or both.
keyVal :: VKey -> VKey -> User -> RealB
keyVal neg pos u = pressedI pos - pressedI neg
 where
   pressedI key = ifB (pressed key) 1 0
   pressed  key = stepper False $
                  keyPress key u -=> True .|. keyRelease key u -=> False


stylusHand u = mkHand (stylus u) (stylusDown u) (stylusUp u)
