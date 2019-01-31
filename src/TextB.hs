-- Text vectors
--
-- Last modified Wed Jul 09 09:06:12 1997

module TextB where

import qualified Text as T
import Behavior

type TextB = Behavior T.TextT

simpleText = lift1 T.simpleText
boldT      = lift1 T.boldT
italicT    = lift1 T.italicT

-- No font behaviors for now

textFont f = lift1 (T.textFont f)


