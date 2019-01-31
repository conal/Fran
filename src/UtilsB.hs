-- Ad hoc collection of definitions. These are here for introductory use,
-- to make it easier to do some simple things. I'd like to make it so that
-- kids can use this simple vocabulary.
--
-- Last modified Thu Jul 24 09:40:56 1997

module UtilsB where

import qualified StaticTypes as S
import Force
import BaseTypes
import Behavior
import VectorSpaceB
import Event
import Vector2B
import Point2B
import ColorB
import TextB
import Transform2B
import ImageB
import User
import Integral
import Interaction
import HSpriteLib

-- Temporarily
import IOExtensions (unsafePerformIO)

-- Move

-- move dp thing = translate2 dp *% thing

moveXY dx dy thing = move (vector2XY dx dy) thing

-- Resize
bigger = stretch

smaller sc = bigger (1/sc)

-- biggerXY scx scy im = scale2 (vector2XY scx scy) *% im
-- smallerXY scx scy = biggerXY (1/scx) (1/scy)

-- turnLeft frac im = rotate2 (frac * pi) *% im
-- turnRight frac = turnLeft (-frac)

-- No-ops for now.  Sorry
-- turnLeft frac im = im
-- turnRight frac im = im

-- Oscillates between -1 and 1
wiggle  = sin (pi * timeSince 0)        -- Zero is Bogus!!! ###

-- Ditto, but off phase
-- waggle = later 0.5 wiggle
waggle  = cos (pi * timeSince 0)        -- Bogus!!! ###

wiggleRange lo hi =
 lo + (hi-lo) * (wiggle+1)/2

-- Shift a behavior to be later or earlier, faster or slower

later, earlier :: TimeB -> Behavior a -> Behavior a

later dt b = b `timeTransform` (timeSince 0 - dt)  -- Bogus zero!!! ###
earlier dt = later (-dt)

faster x b = b `timeTransform` (timeSince 0 * x)  -- Bogus!!! ###
slower x   = faster (1/x)

-- Import a single bitmap from a file.  Simple case of flipbook ImageB

importBitmapWithSize :: String -> (ImageB, RealVal, RealVal)
importBitmapWithSize fileName =
  (flipImage book 0, fromInt w / bitmapPixelsPerLength
                   , fromInt h / bitmapPixelsPerLength)
 where
  book  = --trace "Making flip book" $
          flipBook surf w h 0 0 1 1
  (w,h) = --trace "Getting surface size" $
          getDDSurfaceSize surf
  surf  = --trace "Making bitmap surface" $
          bitmapDDSurface fileName

importBitmap :: String -> ImageB
importBitmap fileName = imB
 where (imB, width, height) = importBitmapWithSize fileName

bezier = error "bezier not currently implemented -- sorry"
circle = error "circle not currently implemented -- sorry"


-- Continuous show, rendered into an image

showBIm :: Show a => Behavior a -> ImageB
showIm  :: Show a =>          a -> ImageB

showBIm = stringBIm . showB
showIm  = showBIm . constantB

stringBIm :: Behavior String -> ImageB
stringIm  ::          String -> ImageB

stringBIm str = textImage (simpleText str) 0
stringIm      = stringBIm . constantB

-- Synonym

atRate :: (Forceable v, S.VectorSpace v) => Behavior v -> User -> Behavior v
atRate = integral



-- Mouse motion vector, i.e., where the mouse is relative to the origin

mouseMotion :: User -> Vector2B

mouseMotion u = mouse u .-. origin2

-- More specialized 

-- Given an image and a canonical size, stretch the image uniformly so
-- that the size maps exactly onto the window view size.

viewStretch :: Vector2B -> User -> ImageB -> ImageB

viewStretch size u =
  bigger ((wWidth  / iWidth ) `min` (wHeight / iHeight))
  where
    (wWidth, wHeight) = pairBSplit (vector2XYCoords (viewSize u))
    (iWidth, iHeight) = pairBSplit (vector2XYCoords size)

