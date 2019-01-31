{- Colour - just RGB at the moment -}

module Color where

import BaseTypes

data Color 
 = RGB Fraction Fraction Fraction -- all in [0,1]
 | HSL 
     Fraction -- [0,360]
     Fraction -- [0,1]
     Fraction -- [0,1]
   deriving (Eq, Text)

rgb :: Fraction -> Fraction -> Fraction -> Color
rgb = RGB

hsl :: Fraction -> Fraction -> Fraction -> Color
hsl = HSL

{-
 mix* operators are multiplicative colour transformators.

-}

mixRed,mixGreen, mixBlue :: Fraction -> Color -> Color
mixRed f (RGB r g b)   = RGB (f*r) g b
mixGreen f (RGB r g b) = RGB r (f*g) b
mixBlue f (RGB r g b)  = RGB r g (f*b)

-- From the PicBasics module shipped with Yale Hugs
-- Maybe move to AffineSpace class when we have type relations.
interpolateColor :: Color -> Color -> RealVal -> Color
interpolateColor (RGB r1 g1 b1) (RGB r2 g2 b2) t =
 RGB (f r1 r2) (f g1 g2) (f b1 b2)
 where
  f x y = t'*x + t'*y
  t' = 1 - t

gray :: Fraction -> Color
gray g = RGB g g g

white, black, red, green, blue :: Color
lightBlue, royalBlue, yellow, brown :: Color
black = RGB 0 0 0
white = RGB 1 1 1
red   = RGB 1 0 0
green = RGB 0 1 0
blue  = RGB 0 0 1
lightBlue = RGB 0 1 1
royalBlue = RGB 0 0 0.5
yellow    = RGB 1 1 0
brown     = RGB 0.5 0 0

clamp mi ma v = max mi (min ma v)

stronger :: Fraction -> Color -> Color
stronger d (HSL h s l) = HSL (clamp 0 360 (h+d)) s l
stronger d (RGB r g b) =
 let
  (h,s,l) = rgb_to_hsl r g b
 in
 HSL (clamp 0 360 (h+d)) s l

duller :: Fraction -> Color -> Color
duller d = stronger (-d)

darker :: Double -> Color -> Color
darker d = brighter (-d)

brighter :: Double -> Color -> Color
brighter d (HSL h s l) = HSL h s (clamp 0 1 (d+l))
brighter d (RGB r g b) = 
 let
  (h,s,l) = rgb_to_hsl r g b
 in
 HSL h s (clamp 0 1 (d+l))

toRGB :: Color -> Color
toRGB c =
  case c of
   RGB _ _ _ -> c
   HSL h s l ->
    let (r,g,b) = hsl_to_rgb h s l in
    RGB r g b

toHSL :: Color  -> Color
toHSL c =
 case c of
  HSL _ _ _ -> c
  RGB r g b ->
   let (h,s,l) = rgb_to_hsl r g b in
   HSL h s l


{-
 From ColourmapImpl.lhs in Haggis distrib. (boxified version),
 which again was `inspired' by Foley&van Dam.
-}

hsl_to_rgb :: Fraction 
	   -> Fraction 
           -> Fraction 
	   -> (Fraction,
	       Fraction,
	       Fraction)
hsl_to_rgb h s l =
 (value m1 m2 (h + 120),
  value m1 m2 h,
  value m1 m2 (h - 120))
 where
  m2 =
   if l <= 0.5 then
      l * (1 + s)
   else
      l + s - (l * s)
  m1 =
   (2 * l) - m2

  value n1 n2 hue =
   let
    hue' =
     if hue > 360 then
        hue - 360
     else if hue < 0 then
        hue + 360
     else
        hue
   in
   if hue' < 60 then
      n1 + (n2 - n1) * hue' / 60
   else if hue' < 180 then
      n2
   else if hue' < 240 then
      n1 + (n2 - n1) *
      (240   - hue') / 60
   else
      n1

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
    (h,s,l)




