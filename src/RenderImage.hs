-- Various routines to create and draw into surfaces.
--
-- Last modified Wed Oct 08 16:59:06 1997

module RenderImage where

import BaseTypes
import Point2
import Color
import Transform2
import VectorSpace
import Vector2
import qualified Font
import Text
import Win32 hiding (readFile, writeFile)
import Win32 (unsafePerformIO)
import HSpriteLib

import Trace

-- The RealVals are the x and y motion, and the pixels are the sprite
-- upperLeft w.r.t. the origin

type SurfaceUL = (HDDSurface, Pixels, Pixels, RealVal, RealVal)

-- text, angle, color, stretch

renderText :: TextT -> Color -> Transform2 -> SurfaceUL
renderText (TextT (Font.Font fam bold italic) str) color
	   xf@(Transform2 (Vector2XY x y) stretch angle) =
 -- HSurfaces are immutable, and there are no visible side-effects.
 -- ## BUT memory management is crucially important.  BUGGY!!
 unsafePerformIO $
 --putStrLn ("renderText " ++ show xf)>>
 createFont fontWidth 0
	    escapement			-- escapement
	    escapement                  -- orientation
	    weight
	    italic False False    -- italic, underline, strikeout
	    aNSI_CHARSET
	    oUT_DEFAULT_PRECIS
	    cLIP_DEFAULT_PRECIS
	    dEFAULT_QUALITY
	    dEFAULT_PITCH
	    gdiFam                         >>= \ hf ->

 -- First figure out what size to make the new surface.

 (withScratchHDC $ \scratchHDC ->
   selectFont scratchHDC hf	>>
   getTextExtentPoint32 scratchHDC str)    >>= \ horizSize->

 -- Sadly, getTextExtentPoint32 seems to ignore escapement and
 -- orientation, so we have to adjust.
 let ((surfWidth, surfHeight), (dulx,duly)) = rotateSize horizSize angle in
 (withNewHDDSurfaceHDC surfWidth surfHeight backColorREF $ \hdc ->
   selectFont hdc hf                   >>
   setBkColor hdc backColorREF	       >>
   setTextColor hdc (asColorRef color) >>
   -- setTextAlign hdc tA_TOP	       >>
   setTextAlign hdc (tA_TOP `orb` tA_LEFT)    >>
   -- setTextAlign hdc tA_CENTER	       >>
   -- setTextAlign hdc (tA_BOTTOM `orb` tA_LEFT)	       >>
   textOut hdc dulx (- duly) str) >>= \hSurface ->
 deleteFont hf                             >>

{-
 putStrLn ("renderText: str " ++ show str ++
	   ", color " ++ show color ++
	   ", stretch " ++ show stretch ++
	   ", angle " ++ show angle ++
	   ", fontWidth " ++ show fontWidth ++
	   ", horizSize " ++ show horizSize ++
	   ", surfSize " ++ show (surfWidth,surfHeight) ++
	   ", dul " ++ show (dulx,duly) ++
	   ", esc " ++ show escapement ++
	   "" ) >>
-}
 return (hSurface, - fromInt surfWidth `div` 2,
		   - fromInt surfHeight `div` 2, x, y)
 where
  backColor | color /= black =  black
	    | otherwise      =  white
  backColorREF = asColorRef backColor
  -- The "20" was empirically determined
  fontWidth  = round (screenPixelsPerLength * stretch / 5.0)
  escapement = round (angle * 1800/pi)	-- hundredths of a degree
  weight  | bold      = fW_BOLD
          | otherwise = fW_NORMAL
  gdiFam =
   case fam of
     Font.System     -> "System" -- this is going to break ..
     Font.TimesRoman -> "Times New Roman"
     Font.Courier    -> "Courier New"
     Font.Arial      -> "Arial"
     Font.Symbol     -> "Symbol"


-- Gives new size, and the position of the upper left w.r.t the upper left
-- of the new box.
rotateSize :: (Int, Int) -> Radians -> ((Int, Int), (Int, Int))

rotateSize (width, height) rotAngle =
  -- Think of the bounding box as centered at the origin.  Rotate the
  -- upper right and lower right points, and see which stick out further
  -- horizontally and vertically.
  let
      w2 = fromInt width / 2			-- half width and height
      h2 = fromInt height / 2
      ur = point2XY w2 h2		-- upper right and lower right
      lr = point2XY w2 (-h2)
      xf = rotate2 rotAngle
      Point2XY urx' ury' = xf *% ur     -- x,y of rotated versions
      Point2XY lrx' lry' = xf *% lr
      -- Two cases: new width&height determined by ur&lr or by lr&ur
      (w2',h2')  |  abs urx' > abs lrx'  =  (abs urx', abs lry')
		 |  otherwise		 =  (abs lrx', abs ury')
      size' = (round (w2' * 2), round (h2' * 2))
      -- ul' is (-lrx, -lry), and new box's upper left is (-w2',h2')
      dul = (round (-lrx' - (- w2')), round (-lry' - h2'))
  in
      --trace ("angle " ++  show rotAngle ++ ".  ur = " ++ show ur ++ ".  ur' = " ++ show (xf *% ur) ++ "\n") $
      (size', dul)
	


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
    -- the width should be 2 instead of 1
    surf <- newPlainDDrawSurface (width + 1) (height + 1) colRef
    -- Clear the surface first, since we're going to draw
    -- Maybe newPlainDDrawSurface should do that
    clearDDSurface surf colRef
    withDDrawHDC surf f
    return surf

-----------------------------------------------------------------
-- renderCircle: rendering the unit size circle
-----------------------------------------------------------------

renderCircle :: Color -> Transform2 -> SurfaceUL
renderCircle color (Transform2 (Vector2XY x y) sc _) =
  unsafePerformIO $
    withNewHDDSurfaceHDC
      pixWidth pixHeight (asColorRef black) (\ hdc ->
        -- ## WithColor seems pretty heavyweight to use here
        withColor hdc color $
	ellipse hdc 0 0 pixWidth pixHeight)
    >>= \ hDDSurface -> return (hDDSurface, - pixRadius, - pixRadius, x, y)
  where
    radius    = abs sc
    pixRadius = round (radius * screenPixelsPerLength)
    pixWidth  = 2 * pixRadius
    pixHeight = pixWidth

-----------------------------------------------------------------
-- renderPolyline, renderPolygon, renderPolyBezier
-----------------------------------------------------------------

renderPolyline   :: [Point2] -> Color -> Transform2 -> SurfaceUL
renderPolygon    :: [Point2] -> Color -> Transform2 -> SurfaceUL
renderPolyBezier :: [Point2] -> Color -> Transform2 -> SurfaceUL

renderPolyline   = renderPoly polyline
renderPolygon    = renderPoly polygon
renderPolyBezier = renderPoly polyBezier

renderPoly :: (HDC -> [POINT] -> IO ()) -> [Point2] -> Color ->
	      Transform2 -> SurfaceUL
renderPoly polyF pts color xf@(Transform2 (Vector2XY motX motY) sc rot) =
  unsafePerformIO $
    withNewHDDSurfaceHDC
      (toPixel width) (toPixel height) (asColorRef black) (\ hdc ->
        withColor hdc color $
	polyF hdc (map toPixelPoint2 pts''))
    >>= \ hDDSurface -> return (hDDSurface, toPixel minimumX,
			        toPixel (-maximumY), motX, motY)
  where
    pts' = map (Transform2 zeroVector sc rot *%) pts
    ptsTuples = map point2XYCoords pts'
    xs = map fst ptsTuples
    ys = map snd ptsTuples
    width  = maximum xs - minimumX
    height = maximumY - minimum ys
    pts'' = map (swapCoordSys4Pt (point2XY minimumX maximumY)) pts'
    minimumX = minimum xs
    maximumY = maximum ys

-----------------------------------------------------------------
-- renderLine: takes 2 points
-----------------------------------------------------------------

renderLine :: Point2 -> Point2 -> Color -> Transform2 -> SurfaceUL
renderLine p0 p1 color (Transform2 (Vector2XY motX motY) sc rot) =
  unsafePerformIO $
    withNewHDDSurfaceHDC
      (toPixel width) (toPixel height) (asColorRef black) (\ hdc ->
        withColor hdc color $ do
	  moveToEx hdc x0' y0'
	  lineTo   hdc x1' y1')
    >>= \ hDDSurface -> return (hDDSurface,
			        toPixel (ulx), toPixel (-uly),
			        motX, motY)
  where
    p0' = (Transform2 zeroVector sc rot) *% p0
    p1' = (Transform2 zeroVector sc rot) *% p1
    (x0, y0) = point2XYCoords p0'
    (x1, y1) = point2XYCoords p1'
    (x0', y0') = toPixelPoint2 $ swapCoordSys4Pt newOrigin p0'
    (x1', y1') = toPixelPoint2 $ swapCoordSys4Pt newOrigin p1'
    width  = abs (x1 - x0)
    height = abs (y1 - y0)
    ulx    = x0 `min` x1
    uly	   = y0 `max` y1
    newOrigin = point2XY ulx uly

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


toPixel :: RealVal -> Int
toPixel r = round (r * screenPixelsPerLength)

toPixelPoint2 :: Point2 -> (Int, Int)
toPixelPoint2 pt = let (x, y) = point2XYCoords pt
		   in  (toPixel x, toPixel y)

-----------------------------------------------------------------
-- bitmap -> Fran Coordinate System -> screen
-----------------------------------------------------------------

-- Should really be two constants -- horizontal and vertical.
importPixelsPerLength = 100 :: RealVal

-- Note that screen pixels per world length and bitmap pixels per world
-- length do not have to agree.  If they do, however, the scalings will
-- cancel out in the absence of explicit scaling, which makes for much
-- faster display on video cards that don't do hardware scaling.

screenPixelsPerLength = importPixelsPerLength * (2/3) :: RealVal
