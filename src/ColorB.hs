-- Color behaviors
--
-- Last modified Sat Sep 07 23:23:11 1996

module ColorB where

import qualified Color as C
import Behavior

type ColorB = Behavior C.Color

rgb              = lift3 C.rgb
gray             = lift1 C.gray
mixRed           = lift2 C.mixRed
mixGreen         = lift2 C.mixGreen
mixBlue          = lift2 C.mixBlue
interpolateColor = lift3 C.interpolateColor

white     = lift0 C.white
black     = lift0 C.black
red       = lift0 C.red
green     = lift0 C.green
blue      = lift0 C.blue
lightBlue = lift0 C.lightBlue
royalBlue = lift0 C.royalBlue
yellow    = lift0 C.yellow
brown     = lift0 C.brown
