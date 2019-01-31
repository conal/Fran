-- Models a hand that has a position and can grasp and release.

module Hand where

import Fran
import Win32Key

data WhichHand = LeftH | RightH

-- Which hand, motion, grasp, release, grasping, appearance
data Hand = Hand Point2B (Event ()) (Event ()) BoolB

mkHand :: Point2B -> Event () -> Event () -> Hand
mkHand pos tryGrasp tryRelease =
  Hand pos grasp release grasping
 where
   -- Does debouncing, which is useful for keypress-based hands.
   grasping = stepper False (grasp -=> True .|. release -=> False)
   grasp    = tryGrasp `whenE` notB grasping
   release  = tryRelease `whenE` grasping

renderHand :: WhichHand -> Hand -> ImageB
renderHand lr (Hand pos _ _ grasping) =
   moveTo pos $
   -- Little circle helps guide picking
   (stretch 0.02 $ withColor (ifB grasping green red) $ circle) `over`
   ifB grasping (importHand lr True )
                (importHand lr False)

-- Hand images.  Colored dots for now, but should use good-looking bitmaps.

-- Left/right hand utilities
handName :: WhichHand -> Bool -> String
handName LeftH  True  = "hand_pinch_l_32.bmp"
handName LeftH  False = "hand_relax_l_32.bmp"
handName RightH True  = "hand_pinch_r_32.bmp"
handName RightH False = "hand_relax_r_32.bmp"

importHand :: WhichHand -> Bool -> ImageB
importHand lr grasping = move (origin2 .-. pinchPos) imB
 where
   imB = importBitmap (handName lr grasping)

   pinchPos = point2XY (lrFlip lr (fromImportPixel 14)) (fromImportPixel 9)
   lrFlip LeftH  = id
   lrFlip RightH = negate


-- Some hand builders

type HandGen = User -> Hand

mouseHand, keyHand, stylusHand :: HandGen

mouseHand u = mkHand (mouse u) (lbp u) (lbr u)

keyHand u = mkHand pos (keyPress vK_SHIFT u) (keyRelease vK_SHIFT u)
 where
   pos = origin2 .+^ atRate vel u
   vel = vector2XY (keyVal vK_LEFT vK_RIGHT u)
                   (keyVal vK_DOWN vK_UP    u)

-- -1 when only neg pressed, 1 when only pos, 0 when neither or both.
keyVal :: VKey -> VKey -> User -> RealB
keyVal neg pos u = dirVal pos - dirVal neg
 where
   dirVal key = stepper 0 $
                keyPress key u -=> 1 .|. keyRelease key u -=> 0


stylusHand u = mkHand (stylus u) (stylusDown u) (stylusUp u)

-- Stylus hand if present, otherwise key hand.
stylusOrKeyHand :: HandGen
stylusOrKeyHand u 
  | stylusPresent u = stylusHand u
  | otherwise       = keyHand    u
