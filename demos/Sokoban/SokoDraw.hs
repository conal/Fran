module SokoDraw (toImageB) where

import SokoType
import Sokoban
import Fran
import qualified StaticTypes as S
import Array

----------------------------------------------------------------
-- Media stuff
-----------------------------------------------------------------

floorFlipBook :: HFlipBook
floorFlipBook = flipBook floorSurface 32 32 0 0 5 1
  where 
    floorSurface = bitmapDDSurface "../../Media/SokoFloor.bmp"

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 32 32 0 0 8 2
  where
    sokobanSurface = bitmapDDSurface "../../Media/Sokoban.bmp"

toImageB :: Board -> ([Behavior Pos], BoolB) -> (ImageB, BoolB)
toImageB b (locBs, finalB) = (moveables `over` board, finalB)
  where
    board     = mkBoardImageB b finalB
    moveables = mkMoveables locBs

squareW, squareH :: RealVal
(S.Vector2XY squareW squareH) = flipBookSize floorFlipBook

xx, yy :: Int -> S.RealVal
xx x = squareW * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = squareH * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

mkBoardImageB :: Board -> BoolB -> ImageB
mkBoardImageB b finalB = overs $ [ f x y | x <- [0 .. (maxX - 1)],
                                           y <- [0 .. (maxY - 1)]]
  where
    f x y = 
      let identityB = lift0 $ b ! (x, y)

          f' final Empty  = if final then 4 else 2
          f' final Target = 0
          f' final Wall   = 3

          pageNumberB = lift2 f' finalB identityB
      in  move (lift0 (S.vector2XY (xx x) (yy y)))
               (flipImage floorFlipBook pageNumberB)

mkMoveables :: [Behavior Pos] -> ImageB
mkMoveables locBs = overs $ map toImageB (zip (True : repeat False) locBs)
  where
    posB :: Behavior Pos -> Vector2B
    posB = lift1 f where f (x, y) = S.vector2XY (xx x) (yy y)

    toImageB :: (Bool, Behavior Pos) -> ImageB
    toImageB (isPusher, locB) =
      move (posB locB) (if isPusher then pusherI else boxI)

    pusherI, boxI :: ImageB
    pusherI = flipImage sokobanFlipBook (30 * time)
    boxI    = flipImage floorFlipBook 1
