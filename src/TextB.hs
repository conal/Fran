-- Text vectors
--
-- Last modified Sat Sep 07 23:41:37 1996

module TextB where

import qualified Text as T
import Behavior

type TextB = Behavior T.TextT

simpleText = lift1 T.simpleText
bold       = lift1 T.bold
italic     = lift1 T.italic

-- No font behaviors for now

textFont f = lift1 (T.textFont f)


