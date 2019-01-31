module SokoDraw (toImageB) where

import qualified StaticTypes as S
import Fran
import HSpriteLib
import SokoType
import Array

----------------------------------------------------------------
-- Media stuff (copied from Sokoban1)
-----------------------------------------------------------------

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 16 16 0 0 6 1
  where 
    sokobanSurface = bitmapDDSurface "../../Media/sokoban.bmp"

-----------------------------------------------------------------
-- move (copied from Sokoban1)
-----------------------------------------------------------------

xx, yy :: Int -> S.RealVal
xx x = 0.16 * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = 0.16 * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

xxyy :: Pos -> S.Vector2
xxyy (x, y) = S.Vector2XY (xx x) (yy y)

xxyyB :: Behavior Pos -> Vector2B
xxyyB = lift1 xxyy

-----------------------------------------------------------------
-- toImageB
-----------------------------------------------------------------

toImageB :: Floor -> ([InhabB], Board, BoolB) -> (ImageB, BoolB)
toImageB floor (inhabBs, b, finalB) = (moverables `over` board, finalB)
  where
    board = overs $ map element2ImageB (range boundPos)

    element2ImageB :: Pos -> ImageB
    element2ImageB p =
      move (lift0 (xxyy p))
           (flipImage sokobanFlipBook
	              (lift1 (pageNumber (floor ! p)) finalB))

    pageNumber Empty  final = if final then 5 else 2 -- empty/final
    pageNumber Target final = 1		-- target
    pageNumber Wall   final = 0		-- wall

    moverables = overs $ map inhab2ImageB (zip [0 ..] inhabBs)

    inhab2ImageB :: (Int, InhabB) -> ImageB
    inhab2ImageB (i, (_, posB)) =
      move (xxyyB posB)
           (flipImage sokobanFlipBook (pageNumberB i))

    pageNumberB :: Int -> RealB
    pageNumberB i | i == 0    = 4	-- pusher
                  | otherwise = 3	-- box