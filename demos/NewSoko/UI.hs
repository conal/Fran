-- Sokoban UI

module UI where

import Array
import Win32Key  -- For arrow keys

import Fran
import qualified StaticTypes as S

import Types

-- Swiped from Fifteen.  Should factor into common module somewhere.
sokoIn :: User -> Event Dir
sokoIn u = keyPressAny u `filterE` keyDir

keyDir :: VKey -> Maybe Dir
keyDir vkey = assoc [ (vK_LEFT , West ),
                      (vK_RIGHT, East ),
                      (vK_UP   , North),
                      (vK_DOWN , South) ] vkey

sokoOut :: Config -> LocB -> [LocB] -> IntB -> ImageB
sokoOut (Config fixed _ _) pusherLoc boxLocs left =
  leftIm `over` 
  moveToLoc pusherLoc pusherIm `over`
  overs [moveToLoc bLoc boxIm | bLoc <- boxLocs]  `over`
  overs [ moveToLoc (constantB fLoc) (fixedIm fixedId)
        | (fLoc,fixedId) <- zip (indices fixed) (elems fixed) ]
 where
   pusherIm = flipImage sokobanFlipBook (30 * time)

   leftIm = moveToLoc (pairB (roundB (cols/2)) rows) $
            stretch 2 $
            withColor yellow $
            stringBIm (constantB "out of place: " ++* showB left)

   moveToLoc :: LocB -> ImageB -> ImageB
   moveToLoc = moveTo . locToPoint

fixedIm :: FixedId -> ImageB
fixedIm id = flipImage floorFlipBook (constantB (page id))
 where
   page FixedBlank  = 2
   page FixedTarget = 0
   page FixedWall   = 3

boxIm = flipImage floorFlipBook 1

----------------------------------------------------------------
-- Media stuff
-----------------------------------------------------------------

-- To do: convert these guys to use importFlipBook
floorFlipBook :: HFlipBook
floorFlipBook = flipBook floorSurface 32 32 0 0 5 1
  where 
    floorSurface = bitmapDDSurface "../../Media/SokoFloor.bmp"

sokobanFlipBook :: HFlipBook
sokobanFlipBook = flipBook sokobanSurface 32 32 0 0 8 2
  where
    sokobanSurface = bitmapDDSurface "../../Media/Sokoban.bmp"


-- LocB to Point2B conversion
locToPoint :: LocB -> Point2B
locToPoint locB = point2XY x y
 where
   (col, row) = pairBSplit locB
   row' = rows - 1 - row
   x = (fromIntB col  + 0.5) * pieceWidth
       - puzzleWidth /2
   y = (fromIntB row' + 0.5) * pieceHeight
       - puzzleHeight/2

-- Dimensions
puzzleWidth, puzzleHeight, pieceWidth, pieceHeight :: RealB

puzzleWidth  = pieceWidth  * cols
puzzleHeight = pieceHeight * rows

(pieceWidth,pieceHeight) = (constantB w, constantB h)
 where
   S.Vector2XY h w = flipBookSize floorFlipBook


pieceSize :: Vector2B
pieceSize = vector2XY pieceWidth pieceHeight

squareW, squareH :: RealVal
(S.Vector2XY squareW squareH) = flipBookSize floorFlipBook
