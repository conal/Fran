-- Ad hoc collection of definitions. These are here for introductory use,
-- to make it easier to do some simple things. I'd like to make it so that
-- kids can use this simple vocabulary.
--
-- Last modified Sun Nov 10 16:18:49 1996

module UtilsB where

import qualified StaticTypes as S
import Force

import RBMH


-- Move

move dp thing = translate2 dp *% thing
moveXY dx dy thing   = translate2 (vector2XY dx dy) *% thing

-- Resize
bigger sc thing = uscale2 sc *% thing

smaller sc = bigger (1/sc)

biggerXY scx scy im = scale2 (vector2XY scx scy) *% im

smallerXY scx scy = biggerXY (1/scx) (1/scy)

turnLeft frac im = rotate2 (frac * pi) *% im

turnRight frac = turnLeft (-frac)

-- Oscillates between -1 and 1
wiggle  = sin (pi * time)

-- Ditto, but off phase
wiggle' = later 0.5 wiggle

wiggleRange lo hi =
 lo + (hi-lo) * (wiggle+1)/2

-- Shift a behavior to be later or earlier, faster or slower

later, earlier :: Time -> Behavior a -> Behavior a

later dt b = b `timeTransform` (time - lift0 dt)
earlier dt = later (-dt)

faster x b = b `timeTransform` (time * x)
slower x   = faster (1/x)

-- Continuous show, rendered into an image

showIm :: Show a => Behavior a -> ImageB

showIm = stringBIm . showB

stringIm :: String -> ImageB

stringBIm :: Behavior String -> ImageB

stringIm = stringBIm . lift0

stringBIm = renderedText . simpleText


-- Synonym

atRate :: (Forceable v, S.VectorSpace v) => Behavior v -> Time -> Behavior v
atRate = integral


-- More specialized 

-- Given an image and a canonical size, stretch the image so that the size
-- maps exactly onto the window view size.

viewStretch :: Vector2B -> Time -> ImageB -> ImageB

viewStretch size t0 =
  biggerXY (wWidth  / iWidth ) (wHeight / iHeight)
  where
    (wWidth, wHeight) = pairBSplit (vector2XYCoords (viewSize t0))
    (iWidth, iHeight) = pairBSplit (vector2XYCoords size)

