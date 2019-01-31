-- Color behaviors
--
-- Last modified Mon Sep 09 17:02:43 1996

module ColorB where

import qualified Color as C
import Behavior

type ColorB = Behavior C.Color

hsl              = lift3 C.hsl
rgb              = lift3 C.rgb
gray             = lift1 C.gray
interpolateColor = lift3 C.interpolateColor
stronger         = lift2 C.stronger
duller           = lift2 C.duller
darker           = lift2 C.darker
brighter         = lift2 C.brighter
shade            = lift2 C.shade

white     = lift0 C.white
black     = lift0 C.black
red       = lift0 C.red
green     = lift0 C.green
blue      = lift0 C.blue
lightBlue = lift0 C.lightBlue
royalBlue = lift0 C.royalBlue
yellow    = lift0 C.yellow
brown     = lift0 C.brown
