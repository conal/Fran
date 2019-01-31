--------------------------------------------------------------------------
-- A grid with conversion between locations and points
--------------------------------------------------------------------------

module Grid where

import Fran

rows, cols :: Int
rows = 4
cols = 4

puzzleWidth,  puzzleHeight :: RealB
(puzzleWidth, puzzleHeight) = (2, 2)

pieceWidth, pieceHeight :: RealB
pieceHeight = puzzleHeight / fromInt cols
pieceWidth  = puzzleWidth  / fromInt rows

pieceSize :: Vector2B
pieceSize     = vector2XY pieceWidth pieceHeight
pieceHalfSize = pieceSize ^/ 2


locToCenterPos :: LocB -> Point2B

locToCenterPos loc = point2XY x y
 where
   (col, row) = pairBSplit loc
   row' = constantB (rows - 1) - row
   x = (fromIntB col  + 0.5) * pieceWidth  - puzzleWidth /2
   y = (fromIntB row' + 0.5) * pieceHeight - puzzleHeight/2

posToLoc :: Point2B -> LocB

posToLoc pos = pairB col row
 where
   -- This def comes from symbolically inverting the previous one, taking
   -- round as the inverse of fromIntB
   (x,y) = pairBSplit (point2XYCoords pos)
   col  = roundB ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = roundB ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = constantB (rows - 1) - row'
