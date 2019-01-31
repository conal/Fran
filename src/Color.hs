{- Colour - just RGB at the moment -}

module Color where

import BaseTypes

data Color 
 = RGB RealVal RealVal RealVal -- all in [0,1]
   deriving (Eq, Text)

rgb :: RealVal -> RealVal -> RealVal -> Color
rgb = RGB

mixRed,mixGreen, mixBlue :: RealVal -> Color -> Color
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

gray :: RealVal -> Color
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

