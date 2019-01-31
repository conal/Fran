--------------------------------------------------------------------------
-- A grid with conversion between locations and points
--------------------------------------------------------------------------

module Grid (Loc, LocB, locRange, minLoc, maxLoc, validLoc
            , LocMove, moveWest, moveEast, moveNorth, moveSouth, stayHere
            , addLocMove 
            , locToCenterPos, posToLoc
            , rows, cols
            ) where

import Fran

type Loc     = (Int, Int)                 -- Board location (column, row)
type LocB    = Behavior Loc
type LocMove = (Int, Int)                 -- (dx, dy) each -1, 0, or 1


-- To do: resolve about up as positive or negative.  If it changes, then
-- fix locToCenterPos and posToLoc.

moveWest, moveEast, moveNorth, moveSouth, stayHere :: LocMove
moveWest  = (-1,0) ; moveEast  = (1,0)
moveNorth = (0,-1) ; moveSouth = (0,1)
stayHere  = (0,0)

addLocMove :: Loc -> LocMove -> Loc
addLocMove (x,y) (dx,dy) = (x+dx, y+dy)

locRange :: (Loc,Loc)
minLoc, maxLoc :: Loc
locRange@(minLoc, maxLoc) = ((0,0),(cols-1,rows-1))

validLoc :: Loc -> Bool
validLoc =  inRange locRange

puzzleWidth,  puzzleHeight :: RealB
(puzzleWidth, puzzleHeight) = (2, 2)

pieceWidth, pieceHeight :: RealB
pieceHeight = puzzleHeight / cols
pieceWidth  = puzzleWidth  / rows

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


rows, cols :: Num a => a
rows = 4
cols = 4
