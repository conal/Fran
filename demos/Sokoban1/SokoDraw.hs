module SokoDraw (toImageB) where

import SokoType
import Sokoban(getFloorID, levelOver)
import Fran
import qualified StaticTypes as S
import HSpriteLib

----------------------------------------------------------------
-- Media stuff
-----------------------------------------------------------------

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 16 16 0 0 6 1
  where 
    sokobanSurface = bitmapDDSurface "../../Media/sokoban.bmp"

toImageB :: [Inhab] -> Board -> Behavior World -> (ImageB, BoolB)
toImageB is b wb = (moveables `over` board, finalB)
  where
    inhabsB = lift1 f wb where f (World inhabs) = inhabs
    finalB  = levelOver b wb

    board     = mkBoardImageB b finalB
    moveables = mkMoveables is inhabsB

xx, yy :: Int -> S.RealVal
xx x = 0.16 * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = 0.16 * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

mkBoardImageB :: Board -> BoolB -> ImageB
mkBoardImageB b finalB = overs $ [ f x y | x <- [0 .. (maxX - 1)],
                                           y <- [0 .. (maxY - 1)]]
  where
    f x y = 
      let identityB = lift0 $ getFloorID (x, y) b

          f' final Empty  = if final then 5 else 2
          f' final Target = 0		-- target
          f' final Wall   = 1		-- wall

          pageNumberB = lift2 f' finalB identityB
      in  move (lift0 (S.vector2XY (xx x) (yy y)))
               (flipImage sokobanFlipBook pageNumberB)

mkMoveables :: [Inhab] -> Behavior [Inhab] -> ImageB
mkMoveables dummy inhabsB =
  let listInhabB = listBTrans (length dummy) inhabsB	-- see warning below!
      listImageB = map toImageB listInhabB
  in  overs listImageB
  where
    posB :: Behavior Inhab -> Vector2B
    posB = lift1 f where f (Inhab _ (x, y)) = S.vector2XY (xx x) (yy y)

    pageNumberB :: Behavior Inhab -> RealB
    pageNumberB = lift1 f where f (Inhab w _) | w == 0    = 4	-- pusher
                                              | otherwise = 3	-- box

    toImageB :: Behavior Inhab -> ImageB
    toImageB inhabB =
      move (posB inhabB) (flipImage sokobanFlipBook (pageNumberB inhabB))

----------------------------------------------------------------
-- Warning: the following is generally not possible!
----------------------------------------------------------------

listBTrans :: Int -> Behavior [a] -> [Behavior a]
listBTrans total aB = [ takeIth aB i | i <- [0 .. (total - 1)] ]
  where
    takeIth aB i = lift1 (!! i) aB
