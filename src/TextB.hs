-- Text vectors
--
-- Last modified Mon Sep 16 21:11:14 1996

module TextB where

import qualified Text as T
import Behavior

type TextB = Behavior T.TextT

simpleText = lift1 T.simpleText
boldT      = lift1 T.boldT
italicT    = lift1 T.italicT

-- No font behaviors for now

textFont f = lift1 (T.textFont f)


