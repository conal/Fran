-- Various routines to create and draw into surfaces.  Adapted from
-- ..\RenderImage.hs

module RenderImage where

import BaseTypes
import Point2
import Color
import Transform2
import Vector2
import qualified Font
import Text
import Win32 hiding (readFile, writeFile)
import IOExtensions (unsafePerformIO)
import HSpriteLib

-- text, angle, optional color, stretch

renderText :: TextT -> RealVal -> Color -> RealVal  -> HDDSurface
renderText (TextT (Font.Font fam bold italic) str) angle color stretch =
 -- HSurfaces are immutable, and there are no visible side-effects.
 unsafePerformIO $
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
 putStrLn (--"renderText: str " ++ show str ++
	   --", color " ++ show color ++
	   --", stretch " ++ show stretch ++
	   ", angle " ++ show angle ++
	   --", fontWidth " ++ show fontWidth ++
	   ", horizSize " ++ show horizSize ++
	   ", surfSize " ++ show (surfWidth,surfHeight) ++
	   ", dul " ++ show (dulx,duly) ++
	   -- ", esc " ++ show escapement
	   "" ) >>
-}
 return hSurface
 where
  backColor | color /= black =  black
	    | otherwise      =  white
  backColorREF = asColorRef backColor
  -- The "20" was empirically determined
  fontWidth  = round (20 * stretch)
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
      (urx',ury') = point2XYCoords(xf *% ur) -- x,y of rotated versions
      (lrx',lry') = point2XYCoords(xf *% lr)
      -- Two cases: new width&height determined by ur&lr or by lr&ur
      (w2',h2')  |  abs urx' > abs lrx'  =  (abs urx', abs lry')
		 |  otherwise		 =  (abs lrx', abs ury')
      size' = (round (w2' * 2), round (h2' * 2))
      -- ul' is (-lrx, -lry), and new box's upper left is (-w2',h2')
      dul = (round (-lrx' - (- w2')), round (-lry' - h2'))
  in
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
 do surf <- newPlainDDrawSurface width height colRef
    -- Clear the surface first, since we're going to draw
    clearDDSurface surf colRef
    withDDrawHDC surf f
    return surf

