--------------------------------------------------------------------------
-- A grid with conversion between locations and motion vectors
--------------------------------------------------------------------------

module Grid ( Loc, LocB, Dir(..), addDir
            , locToVecB, locCropped
            , blankLoc0, loc0s
            ) where

import Fran
import qualified StaticTypes as S

type Loc  = (Int, Int)                  -- Board location (column, row)
type LocB = Behavior Loc


data Dir = East | West | North | South

addDir :: Loc -> Dir -> Loc
addDir (x,y) = add
 where
   add West  = (x-1, y  )
   add East  = (x+1, y  )
   add North = (x  , y-1)
   add South = (x  , y+1)


locToVecB :: LocB -> Vector2B
locToVecB = lift1 locToVec
 where
   locToVec (col, row) = S.vector2XY x y
    where
      row' = rows - 1 - row
      x = (fromInt col  + 0.5) * pieceWidth  - puzzleWidth /2
      y = (fromInt row' + 0.5) * pieceHeight - puzzleHeight/2


-- Currently not used, but handy for point-and-click input.  This def
-- comes from symbolically inverting locToVec, taking round as the inverse
-- of fromIntB
vecToLoc :: S.Vector2 -> Loc
vecToLoc pos = (col, row)
 where
   (x,y) = S.vector2XYCoords pos
   col  = round ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = round ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = rows - 1 - row'


-- Image cropped for location, moved to origin
locCropped :: ImageB -> Loc -> ImageB
locCropped im loc = move (- motion) $
                    crop rect       $
                    im
 where
   motion = locToVecB (constantB loc)
   rect   = rectFromCenterSize (origin2 .+^ motion) pieceSize


-- Starting locations
blankLoc0 : loc0s = [ (x,y) | y <- [0 .. rows-1], x <- [0 .. cols-1] ]


-- Dimensions

puzzleWidth,  puzzleHeight, pieceWidth, pieceHeight :: Fractional a => a
puzzleWidth  = 2
puzzleHeight = 2
pieceHeight  = puzzleHeight / cols
pieceWidth   = puzzleWidth  / rows

pieceSize :: Vector2B
pieceSize     = vector2XY pieceWidth pieceHeight
pieceHalfSize = pieceSize ^/ 2

rows, cols :: Num a => a
rows = 4
cols = 4


