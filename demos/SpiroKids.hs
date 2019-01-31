-- Swirling kids.  Use mouse buttons to change path, and key press to turn
-- message on and off

import Fran
import Tutorial (jake, becky, charlotte, pat, delayAnims)

main = displayU spiroKids

spiroKids u =
  moveXY 0 (- height / 2 + 0.25) (
   withColor white (
    stretch 2 (
      (stringBIm (condB quiet (constantB "") message)))))  `over`
  swirl n u
 where
  n       = stepper 0 (scanlE (+) 0 changeN)
  quiet   = toggle False (keyPressAny u)
  changeN = lbp u -=> (-1) .|. rbp u -=> 1
  message = constantB "right or left click" `untilB` changeN -=> showB n
  (width,height) = vector2XYCoords (viewSize u)

swirl n u = 
  delayAnims 0.5 (map (move path)
                      (zipWith stretch
                               (map constantB scales) (cycle kidsL)))
 where
  scales  = [1, (1 - 2 / fromInt numKids) .. -1]
  numKids = 15
  -- shrink to keep heads in window
  size    = (height `min` width) / 2 - 0.5*shrinkage
  path    = size *^ vector2Polar (sin (fromIntB n*time)) time
  (width,height) = vector2XYCoords (viewSize u)


shrinkage :: Fractional a => a
shrinkage = 0.75

kidsL = map (stretch shrinkage) [stretch 1.2 jake, becky, charlotte, pat]

toggle :: Bool -> Event a -> Behavior Bool

toggle bool e =
  constantB bool `untilB` nextE e ==> toggle (not bool)