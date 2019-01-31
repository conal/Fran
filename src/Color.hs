-- Simple color type.
--
-- Last modified Mon Oct 13 13:55:17 1997

module Color(
	Color(..),
	colorRGB, colorHSL,
	colorRGBCoords, colorHSLCoords,
	-- rgb, hsl,			-- old names, phase out
	asColorRef,
	interpolateColorRGB, interpolateColorHSL,
	grey,
	white, black, red, green, blue,
	lightBlue, royalBlue, yellow, brown,
	transformHSL, stronger, duller, darker, brighter, shade
	) where

import BaseTypes
import qualified Win32 (COLORREF, rgb)

-- Representation uses RGB
data Color = ColorRGB Fraction Fraction Fraction -- all in [0,1]
   deriving (Eq, Show)


colorRGBCoords :: Color -> (Fraction,Fraction,Fraction)
colorRGBCoords (ColorRGB r g b) = (r,g,b)

-- Hue varies between 0 and 2pi
colorHSLCoords :: Color -> (RealVal,Fraction,Fraction)
colorHSLCoords (ColorRGB r g b) =
 rgb_to_hsl r g b

colorRGB :: Fraction -> Fraction -> Fraction -> Color
colorRGB = ColorRGB

colorHSL :: Fraction -> Fraction -> Fraction -> Color
colorHSL h s l = colorRGB r g b
 where (r,g,b) = hsl_to_rgb h s l

asColorRef :: Color -> Win32.COLORREF

asColorRef (ColorRGB r g b) = Win32.rgb r' g' b'
 where
  r' = floor (255*r) `mod` 256
  g' = floor (255*g) `mod` 256
  b' = floor (255*b) `mod` 256

-- Maybe move to AffineSpace class when we have type relations.
-- Note that suffix "RGB", meaning interpolation of RGB coordinates
interpolateColorRGB :: Color -> Color -> RealVal -> Color
interpolateColorRGB (ColorRGB r1 g1 b1) (ColorRGB r2 g2 b2) t =
 colorRGB (f r1 r2) (f g1 g2) (f b1 b2)
 where
  f x y = t*x + t'*y
  t' = 1 - t

interpolateColorHSL :: Color -> Color -> RealVal -> Color
interpolateColorHSL col col' t =
 colorHSL (f h h') (f s s') (f l l')
 where
  f x y = t*x + t'*y
  t' = 1 - t
  (h ,s ,l ) = colorHSLCoords col
  (h',s',l') = colorHSLCoords col'

grey :: Fraction -> Color
grey g = ColorRGB g g g


white, black, red, green, blue :: Color
lightBlue, royalBlue, yellow, brown :: Color
black = grey 0
white = grey 1
red   = ColorRGB 1 0 0
green = ColorRGB 0 1 0
blue  = ColorRGB 0 0 1
lightBlue = ColorRGB 0 1 1
royalBlue = ColorRGB 0 0 0.5
yellow    = ColorRGB 1 1 0
brown     = ColorRGB 0.5 0 0



{-
 From ColourmapImpl.lhs in Haggis distrib. (boxified version),
 which again was `inspired' by Foley&van Dam.
 (conal: changed from degrees to radians)
-}

hsl_to_rgb :: Fraction 
	   -> Fraction 
           -> Fraction 
	   -> (Fraction,
	       Fraction,
	       Fraction)
hsl_to_rgb hRadians s l =
 (value m1 m2 (h + 120),
  value m1 m2 h,
  value m1 m2 (h - 120))
 where
  h = hRadians * 180 / pi
  m2 =
   if l <= 0.5 then
      l * (1 + s)
   else
      l + s - (l * s)
  m1 =
   (2 * l) - m2

  value n1 n2 hue =
   if hue' < 60 then
      n1 + (n2 - n1) * hue' / 60
   else if hue' < 180 then
      n2
   else if hue' < 240 then
      n1 + (n2 - n1) *
      (240   - hue') / 60
   else
      n1
    where
     -- Map hue to [0,360)
     hue' = hue - 360 * fromInt (floor (hue / 360))

rgb_to_hsl :: Fraction
	   -> Fraction
	   -> Fraction
	   -> (Fraction,
	       Fraction,
	       Fraction)
rgb_to_hsl r g b =
 let
  ma = max r (max g b)
  mi = min r (min g b)
  l  = (ma + mi) / 2.0
 in
 if ma == mi then  -- achromatic
    (0, 0, 0)
 else --chromatic colour
    let
     d = ma - mi
     s =
      d /
      (if l <= 0.5 then
         ma + mi
      else
         2 - d)
     h' =
       60* (if r == ma then
             (g - b) / d
          else if g == ma then
             2.0 + (b - r) / d
          else if b == ma then
             4.0 + (r - g) / d
          else
             0.0)

     h = if h' < 0 then h' + 1 else h'
    in
    -- Changed from degrees to radians: conal
    (h  * pi / 180 ,s,l)


transformHSL :: RealVal -> Fraction -> Fraction
	     -> Color -> Color

transformHSL dh ds dl color = 
  colorHSL
   (clamp 0 twicePi (h+dh))
   (clamp 0 1 (s+ds)) 
   (clamp 0 1 (l+dl))
  where
   (h, s, l) = colorHSLCoords color
   clamp mi ma v = mi `max` (ma `min` v)

twicePi = 2 * pi

stronger :: Fraction -> Color -> Color
stronger d = transformHSL 0 d d

duller :: Fraction -> Color -> Color
duller d = transformHSL 0 (-d) (-d)

darker :: Fraction -> Color -> Color
darker d = transformHSL 0 d 0

brighter :: Fraction -> Color -> Color
brighter d = transformHSL 0 (-d) 0

shade :: Fraction -> Color -> Color
shade d = transformHSL 0 1 (-d)
