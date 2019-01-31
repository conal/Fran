{-# OPTIONS -#include <windows.h> #-}

-- Various routines to create and draw into surfaces.

-- To do:
-- + factor the rendering functions to remove commonalities.  Almost
--   done.
-- + 

-- Explanation for the "OPTIONS" line from sof:
--   The compiler is trying to be too clever, performing cross-module
--   inlinings of _casm_s (i.e., doing a better job than most (all?)  C
--   compilers :) This is not without its problems; in your case the C code
--   snippet that is inlined from HSpriteLib mentions a typedef (HDC) that
--   is not in scope when compiling RenderImage.hc
--   To work around this, add ...
--   as the first line in RenderImage.hs

module RenderImage where

import BaseTypes
import Point2
import Color
import Transform2
import VectorSpace
import Vector2
import qualified Font
import Text
import Rect
import Win32 hiding (readFile, writeFile)
import IOExts (unsafePerformIO, trace)
import Bits((.|.))
import Int
import HSpriteLib

data SurfaceUL =
  SurfaceUL HDDSurface
            RealVal RealVal             -- upperLeft w.r.t. the origin
                                        --   *without translation*
            RealVal RealVal             -- x and y motion

-- Tiny dummy surface.  Useful for zero scaling and total cropping
newDummySurfaceUL :: IO SurfaceUL
newDummySurfaceUL = do
  hSurface <- withNewHDDSurfaceHDC 1 1 0 $ \ hdc -> return ()
  return (SurfaceUL hSurface 0 0 0 0)


type SurfGen = Rect -> Color -> Transform2 -> SurfaceUL

renderText :: TextT -> SurfGen
renderText text = render (textRenderer text)

textRenderer :: TextT -> Renderer
textRenderer (TextT (Font.Font fam bold italic) string) color
             xf@(Transform2 _ stretch angle) =
  (renderIt, natRect)
 where
   renderIt toPixPr hdc = do
     selectFont hdc font
     setBkColor hdc backColorREF
     setTextColor hdc (asColorRef color)
     setTextAlign hdc (tA_TOP .|. tA_LEFT)
     textOut hdc ulxPix ulyPix string
     deleteFont font
    where
      (ulxPix, ulyPix) = toPixPr textUL

   (natRect, textUL) = textBox font string xf

   -- Make this guy a function of Font.Font and xf
   font = unsafePerformIO $
          createFont fontHeight 0
                     escapement         -- escapement
                     escapement         -- orientation
                     weight
                     italic False False -- italic, underline, strikeout
                     aNSI_CHARSET
                     oUT_DEFAULT_PRECIS
                     cLIP_DEFAULT_PRECIS
                     dEFAULT_QUALITY
                     dEFAULT_PITCH
                     gdiFam
    where
      -- The "/ 5" was empirically determined
      fontHeight = round (screenPixelsPerLength * stretch / 5.0)
      escapement = round (angle * 1800/pi) -- hundredths of a degree
      weight | bold      = fW_BOLD
             | otherwise = fW_NORMAL
      gdiFam = case fam of
                 Font.System     -> "System"
                 Font.TimesRoman -> "Times New Roman"
                 Font.Courier    -> "Courier New"
                 Font.Arial      -> "Arial"
                 Font.Symbol     -> "Symbol"

   -- Computed redundantly with render.  Make a renderer argument.
   -- The next line helps with debugging
   --backColor = yellow
   backColor | color == black  = white
             | otherwise       = black
   backColorREF = asColorRef backColor



-- Gives bounding rectangle, and the position of the upper left
textBox :: HFONT -> String -> Transform2 -> (Rect, Point2)

textBox font string xf@(Transform2 mot _ angle) = (natRect, xf' *% ul)
 where
   (horizWPix, horizHPix) =
     unsafePerformIO $
     withScratchHDC $ \scratchHDC -> do
      selectFont scratchHDC font
      getTextExtentPoint32 scratchHDC string

   -- Should probably move all of this to Rect's (*%) method.

   w2 = fromPixel horizWPix / 2         -- half width and height
   h2 = fromPixel horizHPix / 2

   ll = point2XY (-w2) (-h2)
   lr = point2XY   w2  (-h2)
   ul = point2XY (-w2)   h2
   ur = point2XY   w2    h2

   -- The text has already been scaled, so we just need to rotate and
   -- translate.
   xf' = Transform2 mot 1 angle
   
   natRect = pointsBoundRect (map (xf' *%) [ll,lr,ul,ur])



-- More convenient interfaces.  Put these somewhere else

withDDrawHDC :: HDDSurface -> (HDC -> IO a) -> IO a

withDDrawHDC ddSurf f =
 do hdc <- getDDrawHDC ddSurf
    res <- f hdc
    releaseDDrawHDC ddSurf hdc
    return res

withScratchHDC :: (HDC -> IO a) -> IO a

withScratchHDC f =
 do scratchSurf <- get_g_pScratchSurf
    withDDrawHDC scratchSurf f

withNewHDDSurfaceHDC :: Int -> Int -> COLORREF
                     -> (HDC -> IO ()) -> IO HDDSurface

withNewHDDSurfaceHDC width height colRef f =
 do -- (+1) below is important. e.g. renderLine will sometimes
    -- crash without it (two pixels are adjacent to each other
    -- the width should be 2 instead of 1)
    surf <- newPlainDDrawSurface (width + 1) (height + 1) colRef
    -- Clear the surface first, since we're going to draw
    -- Maybe newPlainDDrawSurface should do that
    clearDDSurface surf colRef
    withDDrawHDC surf f
    return surf

-----------------------------------------------------------------
-- renderCircle: rendering the unit size circle
-----------------------------------------------------------------

renderCircle :: SurfGen
renderCircle = render circleRenderer

circleRenderer color (Transform2 (Vector2XY dx dy) sc _) =
  (renderIt, natRect)
 where
   renderIt toPixPr hdc =
     withColor hdc color $
     ellipse hdc ulxPix ulyPix
                 (ulxPix + twiceRPix) (ulyPix + twiceRPix)
    where
      (ulxPix, ulyPix) = toPixPr ul

   natRect = rectFromCenterSize (point2XY dx dy) (vector2XY twiceR twiceR)

   ul = point2XY (dx - radius) (dy + radius)

   radius  = abs sc
   twiceR  = 2 * radius

   twiceRPix = toPixel32 twiceR


-----------------------------------------------------------------
-- renderPolyline, renderPolygon, renderPolyBezier
-----------------------------------------------------------------

renderPolyline, renderPolygon, renderPolyBezier :: [Point2] -> SurfGen

renderPolyline   = renderPoly polyline
renderPolygon    = renderPoly polygon
renderPolyBezier = renderPoly (renderIfLength bezierOK polyBezier)

-- Only render if there's an appropriate number of points.
renderIfLength pred render hdc points =
 if pred (length points) then
   render hdc points
 else do
   --putStrLn "Warning: wrong number of points"
   return ()

bezierOK nPoints = nPoints > 1 && nPoints `mod` 3 == 1


renderPoly :: (HDC -> [POINT] -> IO ()) -> [Point2] -> SurfGen
renderPoly polyF pts = render (polyRenderer polyF pts)


renderLine :: Point2 -> Point2 -> SurfGen
renderLine p0 p1 = render (lineRenderer p0 p1)

render :: Renderer -> SurfGen
render renderer cropRect color xf@(Transform2 (Vector2XY dx dy) _ _) =
  --trace (show (natRect,textUL) ++ "\n") $
  unsafePerformIO $
  if pixWidth<=0 || pixHeight<=0 then
    newDummySurfaceUL
  else
    withNewHDDSurfaceHDC
      pixWidth pixHeight
      (asColorRef backColor) (\ hdc -> do
        -- This next line is just for debugging.  It shows the ddraw
        -- surface.  
        --rectangle hdc 0 0 (fromInt pixWidth) (fromInt pixHeight)
        renderIt toPixPr hdc)
    >>= \ hDDSurface -> return $
           SurfaceUL hDDSurface (llx-dx) (ury-dy) dx dy
  where
    (renderIt, natRect) = renderer color xf

    RectLLUR (Point2XY llx lly) (Point2XY urx ury) =
      cropRect `intersectRect` natRect

    pixWidth  = toPixel (urx - llx)
    pixHeight = toPixel (ury - lly)

    toPixPr = toPixelPoint2 . swapCoordSys4Pt (point2XY llx ury)
    
    backColor | color == black  = white
              | otherwise       = black


type ToPixPr = Point2 -> (Int32,Int32)

type Renderer = Color -> Transform2
             -> (ToPixPr -> HDC -> IO () , Rect)

lineRenderer :: Point2 -> Point2 -> Renderer
lineRenderer p0 p1 color xf = (renderIt, natRect)
 where
   renderIt toPixPr hdc =
     withColor hdc color $ do
       moveToEx hdc x0Pix y0Pix
       lineTo   hdc x1Pix y1Pix
    where
      (x0Pix, y0Pix) = toPixPr p0'
      (x1Pix, y1Pix) = toPixPr p1'

   p0' = xf *% p0
   p1' = xf *% p1
   (x0, y0) = point2XYCoords p0'
   (x1, y1) = point2XYCoords p1'
   minX = x0 `min` x1; maxX = x0 `max` x1
   minY = y0 `min` y1; maxY = y0 `max` y1

   natRect = rectFromCorners (point2XY minX minY) (point2XY maxX maxY)

polyRenderer :: (HDC -> [POINT] -> IO ()) -> [Point2] -> Renderer
polyRenderer polyF pts color xf = (renderIt, natRect)
 where
   renderIt toPixPr hdc = withColor hdc color $
                          polyF hdc (map toPixPr pts')
   natRect = pointsBoundRect pts'

   pts' = map (xf *%) pts


pointsBoundRect :: [Point2] -> Rect
pointsBoundRect points = 
  rectFromCorners (point2XY minX minY) (point2XY maxX maxY)
 where
   (xs,ys) = unzip (map point2XYCoords points)

   minX = minimum xs; maxX = maximum xs
   minY = minimum ys; maxY = maximum ys



-----------------------------------------------------------------
-- utility functions
-----------------------------------------------------------------

-- This stuff really should be optimized.  We should use pen and brush
-- values and behaviors.
withColor :: HDC -> Color -> IO () -> IO ()
withColor hdc color action = do
  brush    <- createSolidBrush colorRef
  oldBrush <- selectBrush hdc brush
  pen      <- createPen pS_SOLID 1 colorRef
  oldPen   <- selectPen hdc pen
  action
  selectPen   hdc oldPen
  selectBrush hdc oldBrush
  deletePen pen
  deletePen brush
 where
   colorRef = asColorRef color


-- To do: use type classes here to eliminate some functions

toPixel :: RealVal -> Int
toPixel r = round (r * screenPixelsPerLength)

toPixel32 :: RealVal -> Int32
toPixel32 x = intToInt32 (toPixel x)

fromPixel :: Integral int => int -> RealVal
fromPixel p = fromIntegral p / screenPixelsPerLength


toPixelPoint2 :: Point2 -> (Int32, Int32)
toPixelPoint2 pt = (toPixel32 x, toPixel32 y)
 where (x, y) = point2XYCoords pt

-----------------------------------------------------------------
-- bitmap -> Fran Coordinate System -> screen
-----------------------------------------------------------------

-- Should really be two constants -- horizontal and vertical.
importPixelsPerLength = 75 :: RealVal

-- Note that screen pixels per world length and bitmap pixels per world
-- length do not have to agree.  If they do, however, the scalings will
-- cancel out in the absence of explicit scaling, which makes for much
-- faster display on video cards that don't do hardware scaling.

screenPixelsPerLength = 1.2 * importPixelsPerLength :: RealVal
