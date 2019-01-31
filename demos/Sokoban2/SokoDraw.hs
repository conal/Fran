module SokoDraw (toImageB) where

import qualified StaticTypes as S
import Fran
import HSpriteLib
import SokoType
import Array

----------------------------------------------------------------
-- Media stuff (copied from Sokoban1)
-----------------------------------------------------------------

floorFlipBook :: HFlipBook
floorFlipBook = flipBook floorSurface 32 32 0 0 5 1
  where 
    floorSurface = bitmapDDSurface "../../Media/SokoFloor.bmp"

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 32 32 0 0 8 2
  where
    sokobanSurface = bitmapDDSurface "../../Media/Sokoban.bmp"

-----------------------------------------------------------------
-- move (copied from Sokoban1)
-----------------------------------------------------------------

xx, yy :: Int -> S.RealVal
xx x = 0.315 * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = 0.312 * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

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
           (flipImage floorFlipBook (lift1 (pageNumber (floor ! p)) finalB))

    pageNumber Empty  final = if final then 4 else 2 -- empty/final
    pageNumber Target final = 0		-- target
    pageNumber Wall   final = 3		-- wall

    moverables = overs $ map inhab2ImageB (zip (True : repeat False) inhabBs)

    pusherI, boxI :: ImageB
    pusherI = flipImage sokobanFlipBook (15 * time)
    boxI    = flipImage floorFlipBook 1

    inhab2ImageB :: (Bool, InhabB) -> ImageB
    inhab2ImageB (isPusher, (_, posB)) =
      move (xxyyB posB) (if isPusher then pusherI else boxI)
