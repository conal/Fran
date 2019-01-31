--------------------------------------------------------------------------
-- A grid with conversion between locations and motion vectors
--------------------------------------------------------------------------

module Grid (Loc, LocB, locRange, minLoc, maxLoc, allLocs, validLoc
            , pieceSize, pieceHalfSize
            , LocMove, moveWest, moveEast, moveNorth, moveSouth, stayPut
            , addLocMove 
            , locToMotionB, motionToLocB
            , rows, cols, locCrop
            ) where

import Fran
import qualified StaticTypes as S
import Ix

type Loc     = (Int, Int)                 -- Board location (column, row)
type LocB    = Behavior Loc
type LocMove = (Int, Int)                 -- (dx, dy) each -1, 0, or 1


-- To do: resolve about up as positive or negative.  If it changes, then
-- fix locToMotion and motionToLoc.

moveWest, moveEast, moveNorth, moveSouth, stayPut :: LocMove
moveWest  = (-1,0) ; moveEast  = (1,0)
moveNorth = (0,-1) ; moveSouth = (0,1)
stayPut   = (0,0)

addLocMove :: Loc -> LocMove -> Loc
addLocMove (x,y) (dx,dy) = (x+dx, y+dy)

locRange :: (Loc,Loc)
minLoc, maxLoc :: Loc
locRange@(minLoc, maxLoc) = ((0,0),(cols-1,rows-1))

allLocs :: [Loc]
allLocs = range locRange

validLoc :: Loc -> Bool
validLoc =  inRange locRange

puzzleWidth,  puzzleHeight, pieceWidth, pieceHeight :: Fractional a => a
puzzleWidth  = 2
puzzleHeight = 2
pieceHeight = puzzleHeight / cols
pieceWidth  = puzzleWidth  / rows

pieceSize :: S.Vector2
pieceSize     = S.vector2XY pieceWidth pieceHeight
pieceHalfSize = pieceSize S.^/ 2


-- Conversion between locations and vectors.  We can either define via
-- behavior-level functions, or define via static functions and then lift.
-- As of 8/6/97, the latter made Mover be about 20% faster.  Test these
-- alternatives again when the behavior implementation is faster, and with
-- GHC.

locToMotionB :: LocB -> Vector2B

{-
locToMotionB loc = vector2XY x y
 where
   (col, row) = pairBSplit loc
   row' = constantB (rows - 1) - row
   x = (fromIntB col  + 0.5) * pieceWidth  - puzzleWidth /2
   y = (fromIntB row' + 0.5) * pieceHeight - puzzleHeight/2
-}
locToMotionB = lift1 locToMotion

locToMotion (col, row) = S.vector2XY x y
 where
   row' = rows - 1 - row
   x = (fromInt col  + 0.5) * pieceWidth  - puzzleWidth /2
   y = (fromInt row' + 0.5) * pieceHeight - puzzleHeight/2


motionToLocB :: Vector2B -> LocB

{-
motionToLocB pos = pairB col row
 where
   -- This def comes from symbolically inverting the previous one, taking
   -- round as the inverse of fromIntB
   (x,y) = pairBSplit (vector2XYCoords pos)
   col  = roundB ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = roundB ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = constantB (rows - 1) - row'
-}
motionToLocB = lift1 motionToLoc

motionToLoc :: S.Vector2 -> Loc
motionToLoc pos = (col, row)
 where
   -- This def comes from symbolically inverting the previous one, taking
   -- round as the inverse of fromIntB
   (x,y) = S.vector2XYCoords pos
   col  = round ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = round ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = rows - 1 - row'

locCrop :: Loc -> ImageB -> ImageB
locCrop loc = move (constantB (- motion)) . crop (constantB rect)
 where
   rect   = S.rectLLUR ll ur
   ll     = center S..-^ pieceHalfSize
   ur     = center S..+^ pieceHalfSize
   center = S.origin2 S..+^ motion
   motion = locToMotion loc

rows, cols :: Num a => a
rows = 4
cols = 4
