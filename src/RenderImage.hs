-- Converting Picture values into graphical output (Win32 GDI).
--
-- Last modified Sun Sep 08 16:51:45 1996

module RenderImage (draw) where

import qualified Win32 hiding (writeFile,readFile,rgb,drawText)
import Image
import Point2
import Color
import Transform2
import Vector2
import qualified Font
import Text


-- If the x or y pixel separation is below minPixelDrawSize, don't draw.
-- To do: cull off-window bboxed images.

minPixelDrawSize = 1 :: Int

-- To do: use bounding boxes more pervasively.  Define a function easyBBox
-- :: Image -> Maybe (Point2, Point2) that picks off the easy cases:
-- Circle, Square, Bitmap, Bezier, Text, and BBoxed2.

-- Lots of the Win32 stuff down below has been snarfed from the Yale Hugs
-- implementation of Pictures.

draw :: Win32.HDC -> Image -> IO ()
draw hdc im =
 -- we want transparent Text backgrounds
 Win32.setBkMode hdc Win32.tRANSPARENT >>
 draw' identity2 False im
                  
 where
  f :: Transform2 -> Point2 -> Win32.POINT
  f tr pt = case tr *% pt of Point2 x y -> (round x, round y)

  draw' tr hasColor im =
   case im of
     EmptyImage  -> return ()
     Circle ->
        drawCircle hdc tr
     Square ->
       Win32.polygon hdc (map (f tr) squarePoints)
     Bitmap size bmap ->
       drawBitmap hdc
         (f tr (point2XY (-halfWidth) (-halfHeight)))
         (f tr (point2XY   halfWidth    halfHeight ))
         bmap
         where
           (width,height) = vector2XYCoords size
           halfWidth  = width  / 2
           halfHeight = height / 2
     Line p1 p2 ->
        Win32.polyline hdc points
        where points = map (f tr) [p1,p2]
     PolyLine pts ->
        Win32.polygon hdc (map (f tr) pts)
     Bezier p1 p2 p3 ->
        Win32.polyBezier hdc (map (f tr) [origin2,p1,p2,p3])
     RenderedText str ->
        drawText hdc a b str
          where
            (a,b) = f tr origin2
     WithColor c im ->
        if hasColor then
          draw' tr hasColor im
        else
          drawWithColor hdc c (draw' tr True im)
     Over p q ->     
        -- draw p over q
        draw' tr hasColor q >> 
        draw' tr hasColor p
     TransformedImage tr' p ->
        draw' (tr `compose2` tr') hasColor p
     BBoxed2 p1 p2 im ->
       --putStrLn ("bboxed: maxPixelDist = " ++ show maxPixelDist) >>
       if maxPixelDist < minPixelDrawSize then
         return ()
       else
         draw' tr hasColor im
       where
         (x1,y1) = f tr p1
         (x2,y2) = f tr p2
         maxPixelDist = abs (x1-x2) `max` abs (y1-y2)

-- Windows NT supports the function "PlgBlt" which maps a bitmap to
-- a parallelogram.  This isn't supported on Windows'95 so instead
-- we scale the bitmap so that its bottom-left and top-right are
-- in the right place.
-- To do: optimize to use BitBlt where appropriate.
-- (This looks kinda ugly but works ok if there's been no rotation.)
-- drawBitmap :: Win32.HDC -> POINT -> POINT -> HBITMAP -> IO ()
drawBitmap hdc (x0,y0) (x1,y1) bitmap =
  Win32.createCompatibleDC (Just hdc)    >>= \ memdc ->
  Win32.selectBitmap memdc bitmap        >>
  Win32.getBitmapInfo bitmap             >>= \ (_,width,height,_,_,_) ->
{- Sadly, not available:
  Win32.plgBlt hdc p1 p2 p3 memdc 0 0 width height Nothing 0 0 >>
-}
  Win32.stretchBlt hdc x0 y1 (x1-x0) (y0-y1)
		   memdc 0 0 width height Win32.sRCCOPY >>
  Win32.deleteDC memdc


drawText :: Win32.HDC -> Int -> Int -> TextT -> IO ()
drawText hdc x y (TextT fam str) =
  -- no need to get current font before setting new one, since the font is
  -- set locally for each use of Text
-- setFont hdc fam 1 1       >>
 -- putStrLn ("drawing text " ++ str ++ " at " ++ show (x,y)) >>
 Win32.textOut hdc x y str >>
 return ()

{- Need support in Win32.hs beyond Yale0 to do this.
drawText :: Win32.HDC -> Int -> Int -> TextT -> IO ()
drawText hdc x y (TextT (Font.Font fam bold ital) str) =
  -- no need to get current font before setting new one, since the font is
  -- set locally for each use of Text
 Win32.ezCreateFont hdc fam' 150 0 0 False >>= \ hf ->
 Win32.selectFont hdc hf                   >>= \ old ->
 Win32.textOut hdc x y str		   >>
 Win32.selectFont hdc old		   >>
 Win32.deleteFont hf			   >>
 return ()
 where
  fam' =
   (case fam of
     Font.System     -> "System" -- this is going to break ..
     Font.TimesRoman -> "Times New Roman"
     Font.Courier    -> "Courier New"
     Font.Arial      -> "Arial"
     Font.Symbol     -> "Symbol") ++ 
   (if bold then " Bold" else "") ++
   (if ital then " Italic" else "")

-}

drawWithColor hdc (RGB r g b) m =
   -- Sets brush, pen, and text colors.  Oops -- The Win32 "createPen"
   -- function has not Haskell interface.  When it does, restore this code.
   Win32.createSolidBrush c        >>= \ brush ->
   Win32.selectBrush hdc brush     >>= \ oldBrush ->
   --Win32.createPen(pS_SOLID, 2, c) >>= \ pen ->
   --Win32.selectPen hdc pen       >>= \ oldPen ->
   Win32.setTextColor hdc c        >>= \ oldTC ->
   m                               >>
   Win32.setTextColor hdc oldTC	   >>
   --Win32.selectPen hdc oldPen      >>
   --Win32.deletePen pen             >>
   Win32.selectBrush hdc oldBrush  >>
   Win32.deleteBrush brush         >>
   return ()
 where
  c  = Win32.rgb r' g' b'
  r' = floor (255*r) `mod` 256
  g' = floor (255*g) `mod` 256
  b' = floor (255*b) `mod` 256


drawCircle hdc xf =
  case f (Point2 (-circleRadius) (-circleRadius)) of { (x0,y0) ->
  case f (Point2   circleRadius    circleRadius ) of { (x1,y1) ->
  case f (Point2 (-circleRadius)   circleRadius ) of { (x2,y2) ->
  let
    rw = x1 - x0
    rh = y1 - y0
  in
  if True ||
     (rw == rh                  -- circle
      || (x2 == x0 && y2 == y1) -- unrotated ellipse
      || (x2 == x1 && y2 == y0) -- ellipse rotated 90 degrees
      )
  then
    Win32.ellipse hdc x0 y0 x1 y1
  else 
    slowEllipse hdc (compose2 xf (scale2 circleRadius {- h -} ))
  }}}
  where
    f :: Point2 -> Win32.POINT
    f pt = case xf *% pt of Point2 x y -> (round x, round y)

slowEllipse hdc _ = error "slowEllipse"
--slowEllipse hdc (X a b c d e f) = Win32.transformedEllipse hdc a b c d e f
