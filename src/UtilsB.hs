{-# OPTIONS -#include <windows.h> #-}

-- Ad hoc collection of definitions. These are here for introductory use,
-- to make it easier to do some simple things.  I'd like to make it so that
-- kids can use this simple vocabulary.

module UtilsB where

import qualified StaticTypes as S
import Force
import BaseTypes
import Behavior
import GBehavior
import VectorSpaceB
import Event
import Vector2B
import Vector3B
import Point2B
import Point3B
import ColorB
import TextB
import Transform2B
import Transform3B
import ImageB
import SoundB
import GeometryB
import User
import Integral
import qualified RenderImage as R
import HSpriteLib
import Spritify
import IOExts (trace)

-- Move

move :: Transformable2B bv => Vector2B -> bv -> bv
move dp thing = translate2 dp *% thing

moveXY :: Transformable2B bv => RealB -> RealB -> bv -> bv
moveXY dx dy thing = move (vector2XY dx dy) thing

moveTo :: Transformable2B bv => Point2B -> bv -> bv
moveTo p = move (p .-. origin2)


-- Resize

stretch :: Transformable2B bv => RealB -> bv -> bv
stretch sc = (uscale2 sc *%)

-- Removed
--bigger = stretch
--smaller sc = bigger (1/sc)

-- Currently only uniform scaling.  Sorry.

-- biggerXY scx scy im = scale2 (vector2XY scx scy) *% im
-- smallerXY scx scy = biggerXY (1/scx) (1/scy)

turn :: Transformable2B bv => RealB -> bv -> bv
turn angle = (rotate2 angle *%)

-- Eliminated
--turnLeft = turn
--turnRight frac = turnLeft (-frac)

-- Implementation note: because wiggle and waggle are global behavior
-- definitions ("CAFs"), we would get a very nasty space leak if we didn't
-- say "dontMemoizeB" here.  See the proposed fix by the definition of
-- dontMemoizeB in Behavior.lhs.

-- Didn't work, so making primitive.

-- Oscillates between -1 and 1
-- wiggle = dontMemoizeB $ sin (pi * time)

-- Ditto, but off phase
-- waggle = later 0.5 wiggle
-- waggle = dontMemoizeB $ cos (pi * time)

wiggleRange lo hi =
 lo + (hi-lo) * (wiggle+1)/2

-- Shift a behavior to be later or earlier, faster or slower

later, earlier :: GBehavior bv => TimeB -> bv -> bv

later dt b = b `timeTransform` (time - dt)
earlier dt = later (-dt)

faster x b = b `timeTransform` (time * x)
slower x   = faster (1/x)

-- Import a single bitmap from a file.  Simple case of flipbook ImageB

importBitmapWithSize :: String -> (ImageB, RealVal, RealVal)
importBitmapWithSize fileName =
  (flipImage book 0, R.fromImportPixel w, R.fromImportPixel h)
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

importFlipBook :: String -> Int -> Int -> HFlipBook
importFlipBook fileName columns rows =
 --trace (show ((w,h), (pageW,pageH))) $
 flipBook surf pageW pageH 0 0 columns rows
   where
     surf  = bitmapDDSurface fileName
     (w,h) = ddSurfaceSize surf
     pageW = w `div` fromInt columns
     pageH = h `div` fromInt rows


-- Import a .wav file.  Possibly repeat
importWave :: String -> Bool -> SoundB
importWave fileName repeat = bufferSound (waveDSBuffer fileName) repeat

-- Parse a DDSurface into a bunch of flip books.  The DDSurface is assumed
-- to be a vertical concatenation of flip books, each of which is
-- rectangular arrays of images.  These arrays are all presumed to fill up
-- the whole DDSurface width.  The given list of pairs specifies the
-- number of columns and rows of each flip book, starting from the given
-- vertical pixel number (with the top pixel being zero).  If some or all
-- of your surface does not fit this format, you can still use the
-- flipBook construction function directly.

parseFlipBooks :: [(Int, Int)] -> Pixels -> HDDSurface -> [HFlipBook]
parseFlipBooks descrs top surf = loop descrs top
 where
  (surfWidth, _) = ddSurfaceSize surf
  loop [] _  = []

  loop (descr : descrs') top = book : loop descrs' top'
    where
      (book, top') = oneBook descr top

      oneBook :: (Int, Int) -> Pixels -> (HFlipBook, Pixels)
      oneBook (columns, rows) top =
       (flipBook surf size size 0 top columns rows, bottom)
       where
         -- The fromInt's here are bogus but necessary for now :-(
	 size = surfWidth `div` fromInt columns
	 bottom = top + size * fromInt rows


-- Continuous show, rendered into an image

showBIm :: Show a => Behavior a -> ImageB
showIm  :: Show a =>          a -> ImageB

showBIm = stringBIm . showB
showIm  = showBIm . constantB

stringBIm :: Behavior String -> ImageB
stringIm  ::          String -> ImageB

stringBIm str = textImage (simpleText str)
stringIm      = stringBIm . constantB

-- More specialized 

-- Given an image and a canonical size, stretch the image uniformly so
-- that the size maps exactly onto the window view size.

viewStretch :: Vector2B -> User -> ImageB -> ImageB

viewStretch size u =
  stretch ((wWidth  / iWidth ) `min` (wHeight / iHeight))
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
countE e = stepAccum 0 (e -=> (+ 1))
-- Or:
--countE e = stepper 0 (scanlE (\ c _ -> c + 1) 0 e)

-- Oh: here's another formulation, inspired by John P:
-- countE e = stepper 0 (withElemE_ e [1 ..])

-- Import an X mesh file to make a geometry.  Currently presumes that the
-- X file is a simple mesh.
importX :: String -> GeometryB
importX = meshG . meshBuilder

-- Move, stretch and turn in 3D
move3 :: Vector3B -> GeometryB -> GeometryB
move3 dp = (translate3 dp **%)

moveXYZ :: RealB -> RealB -> RealB -> GeometryB -> GeometryB
moveXYZ dx dy dz = move3 (vector3XYZ dx dy dz)

moveTo3 :: Point3B -> GeometryB -> GeometryB
moveTo3 p = move3 (p .-.# origin3)

stretch3 :: RealB -> GeometryB -> GeometryB
stretch3 sc = (uscale3 sc **%)

turn3 :: Transformable3B a => Vector3B -> RealB -> a -> a
turn3 axis angle = (rotate3 axis angle **%)


-- Display-related

displayU :: (User -> ImageB) -> IO ()
displayU imF = displayUs [imF]

userDelay :: GBehavior bv => bv -> User -> bv
userDelay bv u = later (constantB (userStartTime u)) bv

displays :: [ImageB] -> IO ()
displays = displayUs . map userDelay

display :: ImageB -> IO ()
display imB = displays [imB]

displayGs :: [GeometryB] -> IO ()
displayGs  = displayGUs . map const

displayG :: GeometryB -> IO ()
displayG g   = displayGs [g]

displayGUs :: [User -> GeometryB] -> IO ()
displayGUs = displayUs . map imF
 where
   imF gf = \ u -> renderGeometry (gf u) defaultCamera

displayGU :: (User -> GeometryB) -> IO ()
displayGU gf = displayGUs [gf]

