-- Color behaviors
--
-- Last modified Thu Nov 07 14:34:11 1996

module ColorB where

import qualified Color as C
import Fuzzy
import Behavior

type ColorB = Behavior C.Color

hsl              = lift3 C.hsl (noI "hsl")
rgb              = lift3 C.rgb (noI "rgb")
gray             = lift1 C.gray (noI "gray")
interpolateColor = lift3 C.interpolateColor (noI "interpolateColor")
stronger         = lift2 C.stronger (noI "stronger")
duller           = lift2 C.duller (noI "duller")
darker           = lift2 C.darker (noI "darker")
brighter         = lift2 C.brighter (noI "brighter")
shade            = lift2 C.shade (noI "shade")

white     = lift0 C.white
black     = lift0 C.black
red       = lift0 C.red
green     = lift0 C.green
blue      = lift0 C.blue
lightBlue = lift0 C.lightBlue
royalBlue = lift0 C.royalBlue
yellow    = lift0 C.yellow
brown     = lift0 C.brown
