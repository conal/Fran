module SokoDraw (toImageB) where

import SokoType
import Sokoban(getFloorID, levelOver)
import Fran
import qualified StaticTypes as S
import HSpriteLib

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

toImageB :: [Inhab] -> Board -> Behavior World -> (ImageB, BoolB)
toImageB is b wb = (moveables `over` board, finalB)
  where
    inhabsB = lift1 f wb where f (World inhabs) = inhabs
    finalB  = levelOver b wb

    board     = mkBoardImageB b finalB
    moveables = mkMoveables is inhabsB

xx, yy :: Int -> S.RealVal
xx x = 0.315 * (fromIntegral x - (fromIntegral maxX / 2.0) + 0.5)
yy y = 0.312 * (fromIntegral y - (fromIntegral maxY / 2.0) + 0.5)

mkBoardImageB :: Board -> BoolB -> ImageB
mkBoardImageB b finalB = overs $ [ f x y | x <- [0 .. (maxX - 1)],
                                           y <- [0 .. (maxY - 1)] ]
  where
    f x y = 
      let identityB = lift0 $ getFloorID (x, y) b

          f' final Empty  = if final then 4 else 2
          f' final Target = 0		-- target
          f' final Wall   = 3		-- wall

          pageNumberB = lift2 f' finalB identityB
      in  move (lift0 (S.vector2XY (xx x) (yy y)))
               (flipImage floorFlipBook pageNumberB)

mkMoveables :: [Inhab] -> Behavior [Inhab] -> ImageB
mkMoveables dummy inhabsB =
  let (pusher:box) = listBTrans (length dummy) inhabsB	-- see warning below!
      listImageB = toImageB True pusher : map (toImageB False) box
  in  overs listImageB
  where
    posB :: Behavior Inhab -> Vector2B
    posB = lift1 f where f (Inhab _ (x, y)) = S.vector2XY (xx x) (yy y)

    pusherI, boxI :: ImageB
    pusherI = flipImage sokobanFlipBook (15 * time)
    boxI    = flipImage floorFlipBook 1

    toImageB :: Bool -> Behavior Inhab -> ImageB
    toImageB isPusher inhabB =
      move (posB inhabB) (if isPusher then pusherI else boxI)

----------------------------------------------------------------
-- Warning: the following is generally not possible!
----------------------------------------------------------------

listBTrans :: Int -> Behavior [a] -> [Behavior a]
listBTrans total aB = [ takeIth aB i | i <- [0 .. (total - 1)] ]
  where
    takeIth aB i = lift1 (!! i) aB
