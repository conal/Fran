-- Ad hoc collection of definitions. These are here for introductory use,
-- to make it easier to do some simple things. I'd like to make it so that
-- kids can use this simple vocabulary.
--
-- Last modified Fri Oct 10 09:00:28 1997

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
import GeometryB
import User
import Integral
import Interaction
import HSpriteLib


-- Move

move dp thing = translate2 dp *% thing
moveXY dx dy thing = move (vector2XY dx dy) thing

-- Resize

stretch, bigger, smaller :: RealB -> ImageB -> ImageB

stretch = bigger
bigger sc = (uscale2 sc *%)
smaller sc = bigger (1/sc)

-- Currently only uniform scaling.  Sorry.

-- biggerXY scx scy im = scale2 (vector2XY scx scy) *% im
-- smallerXY scx scy = biggerXY (1/scx) (1/scy)

turnLeft frac im = rotate2 (frac * pi) *% im
turnRight frac = turnLeft (-frac)

-- Oscillates between -1 and 1
wiggle  = sin (pi * time)

-- Ditto, but off phase
-- waggle = later 0.5 wiggle
waggle  = cos (pi * time)

wiggleRange lo hi =
 lo + (hi-lo) * (wiggle+1)/2

-- Shift a behavior to be later or earlier, faster or slower

later, earlier :: TimeTransformable bv => TimeB -> bv -> bv

later dt b = b `timeTransform` (time - dt)
earlier dt = later (-dt)

faster x b = b `timeTransform` (time * x)
slower x   = faster (1/x)

-- Import a single bitmap from a file.  Simple case of flipbook ImageB

importBitmapWithSize :: String -> (ImageB, RealVal, RealVal)
importBitmapWithSize fileName =
  (flipImage book 0, fromInt w / importPixelsPerLength
                   , fromInt h / importPixelsPerLength)
 where
  book  = --trace "Making flip book" $
          flipBook surf w h 0 0 1 1
  (w,h) = --trace "Getting surface size" $
          ddSurfaceSize surf
  surf  = --trace "Making bitmap surface" $
          bitmapDDSurface fileName

importBitmap :: String -> ImageB
importBitmap fileName = imB
 where (imB, width, height) = importBitmapWithSize fileName



-- Continuous show, rendered into an image

showBIm :: Show a => Behavior a -> ImageB
showIm  :: Show a =>          a -> ImageB

showBIm = stringBIm . showB
showIm  = showBIm . constantB

stringBIm :: Behavior String -> ImageB
stringIm  ::          String -> ImageB

stringBIm str = textImage (simpleText str)
stringIm      = stringBIm . constantB

-- Synonym

atRate :: (Forceable v, S.VectorSpace v) => Behavior v -> User -> Behavior v
atRate = integral



-- Mouse motion vector, i.e., where the mouse is relative to the origin

mouseMotion :: User -> Vector2B

mouseMotion u = mouse u .-. origin2

-- Toggle between true and false on event occurrences.

toggle :: Event a -> Event b -> BoolB
toggle go stop =
  stepper False (  go   -=> True
               .|. stop -=> False)

-- Button states
leftButton, rightButton :: User -> BoolB
leftButton  u = toggle (lbp u) (lbr u)
rightButton u = toggle (rbp u) (rbr u)

-- More specialized 

-- Given an image and a canonical size, stretch the image uniformly so
-- that the size maps exactly onto the window view size.

viewStretch :: Vector2B -> User -> ImageB -> ImageB

viewStretch size u =
  bigger ((wWidth  / iWidth ) `min` (wHeight / iHeight))
  where
    (wWidth, wHeight) = vector2XYCoords (viewSize u)
    (iWidth, iHeight) = vector2XYCoords size


-- Convert a user-based event to one that produces the next user.
nextUser :: (User -> Event a) -> (User -> Event (a,User))
nextUser f u = f u `afterE` u

-- Discard the original event data
nextUser_ :: (User -> Event a) -> (User -> Event User)
nextUser_ f u = nextUser f u ==> snd


-- Count the number of event occurrences
countE :: Event a -> Behavior Int
countE e = stepper 0 (scanlE (\ c _ -> c + 1) 0 e)


-- Import an X mesh file to make a geometry.  Currently presumes that the
-- X file is a simple mesh.
importX :: String -> GeometryB
importX = meshG . meshBuilder
