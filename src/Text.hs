-- Text type
--
-- Last modified Mon Sep 16 21:10:37 1996

module Text where

import qualified Font

data TextT = TextT Font.Font String deriving Text

simpleText :: String -> TextT
simpleText str = TextT Font.system str

boldT       :: TextT -> TextT
boldT (TextT f t) = TextT (Font.bold f) t

italicT     :: TextT -> TextT
italicT (TextT f t) = TextT (Font.italic f) t

textFont :: Font.Font -> TextT -> TextT
textFont f (TextT _ t) = TextT f t

