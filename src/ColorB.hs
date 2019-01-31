-- Color behaviors
--
-- Last modified Wed Jul 09 08:50:07 1997

module ColorB where

import qualified Color as C
import Behavior

type ColorB = Behavior C.Color

colorHSL              = lift3 C.colorHSL
colorRGB              = lift3 C.colorRGB
colorHSLCoords        = lift1 C.colorHSLCoords
colorRGBCoords        = lift1 C.colorRGBCoords
grey		      = lift1 C.grey
asColorRef	      = lift1 C.asColorRef
interpolateColorRGB   = lift3 C.interpolateColorRGB
interpolateColorHSL   = lift3 C.interpolateColorHSL
stronger	      = lift2 C.stronger
duller		      = lift2 C.duller
darker		      = lift2 C.darker
brighter	      = lift2 C.brighter
shade		      = lift2 C.shade

white     = constantB C.white
black     = constantB C.black
red       = constantB C.red
green     = constantB C.green
blue      = constantB C.blue
lightBlue = constantB C.lightBlue
royalBlue = constantB C.royalBlue
yellow    = constantB C.yellow
brown     = constantB C.brown

