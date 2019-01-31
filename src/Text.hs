-- Text type
--
-- Last modified Mon Oct 20 15:54:15 1997

module Text(
	TextT(..),
	simpleText, boldT, italicT, textFont
	) where

import qualified Font

data TextT = TextT Font.Font String deriving Show

simpleText :: String -> TextT
simpleText str = TextT Font.timesRoman str

boldT       :: TextT -> TextT
boldT (TextT f t) = TextT (Font.bold f) t

italicT     :: TextT -> TextT
italicT (TextT f t) = TextT (Font.italic f) t

textFont :: Font.Font -> TextT -> TextT
textFont f (TextT _ t) = TextT f t

