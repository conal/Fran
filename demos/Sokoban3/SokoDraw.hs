module SokoDraw (toImageB) where

import SokoType
import Sokoban
import Fran
import qualified StaticTypes as S
import HSpriteLib
import Array

----------------------------------------------------------------
-- Media stuff
-----------------------------------------------------------------

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 16 16 0 0 6 1
  where 
    sokobanSurface = bitmapDDSurface "../../Media/sokoban.bmp"

toImageB :: Board -> ([Behavior Pos], BoolB) -> (ImageB, BoolB)
toImageB b (locBs, finalB) = (moveables `over` board, finalB)
  where
    board     = mkBoardImageB b finalB
    moveables = mkMoveables locBs

xx, yy :: Int -> S.RealVal
xx x = 0.16 * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = 0.16 * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

mkBoardImageB :: Board -> BoolB -> ImageB
mkBoardImageB b finalB = overs $ [ f x y | x <- [0 .. (maxX - 1)],
                                           y <- [0 .. (maxY - 1)]]
  where
    f x y = 
      let identityB = lift0 $ b ! (x, y)

          f' final Empty  = if final then 5 else 2
          f' final Target = 1
          f' final Wall   = 0

          pageNumberB = lift2 f' finalB identityB
      in  move (lift0 (S.vector2XY (xx x) (yy y)))
               (flipImage sokobanFlipBook pageNumberB)

mkMoveables :: [Behavior Pos] -> ImageB
mkMoveables locBs = overs $ map toImageB (zip (4 : repeat 3) locBs)
  where
    posB :: Behavior Pos -> Vector2B
    posB = lift1 f where f (x, y) = S.vector2XY (xx x) (yy y)

    toImageB :: (RealB, Behavior Pos) -> ImageB
    toImageB (pageNumberB, locB) =
      move (posB locB) (flipImage sokobanFlipBook pageNumberB)
