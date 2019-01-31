-- Text type
--
-- Last modified Sat Sep 07 23:36:19 1996

module Text where

import qualified Font

data TextT = TextT Font.Font String deriving Text

simpleText :: String -> TextT
simpleText str = TextT Font.system str

bold       :: TextT -> TextT
bold (TextT f t) = TextT (Font.bold f) t

italic     :: TextT -> TextT
italic (TextT f t) = TextT (Font.italic f) t

textFont :: Font.Font -> TextT -> TextT
textFont f (TextT _ t) = TextT f t

