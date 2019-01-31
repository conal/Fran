-- Text vectors
--
-- Last modified Thu Nov 07 14:34:10 1996

module TextB where

import qualified Text as T
import Fuzzy
import Behavior

type TextB = Behavior T.TextT

simpleText = lift1 T.simpleText (noI "simpleText")
boldT      = lift1 T.boldT (noI "boldT")
italicT    = lift1 T.italicT (noI "italicT")

-- No font behaviors for now

textFont f = lift1 (T.textFont f) (noI "textFont")


