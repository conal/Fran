module SokoType where

import Array

-----------------------------------------------------------------
-- Board
-----------------------------------------------------------------

data FloorID = Empty | Target | Wall deriving (Show, Eq)
type Weight  = Int
type Pos     = (Int, Int)
type Board   = Array Pos FloorID

-----------------------------------------------------------------
-- Force/Direction: type for event from outside
-----------------------------------------------------------------

data Direction = North | South | West | East
type Force     = Weight

maxX, maxY :: Int
maxX = 19
maxY = 12

pusherForce :: Force
pusherForce = 1

boxWeight, pusherWeight :: Weight
boxWeight = 1
pusherWeight = 0

-----------------------------------------------------------------
-- getNeighborPos
-----------------------------------------------------------------

boundPos :: (Pos, Pos)
boundPos = ((0, 0), (maxX - 1, maxY - 1))

getNeighborPos :: Pos -> Direction -> Maybe Pos
getNeighborPos (x, y) d =
  case d of
    North -> if y + 1 >= maxY then Nothing else Just (x, y + 1)
    South -> if y - 1 < 0     then Nothing else Just (x, y - 1)
    West  -> if x - 1 < 0     then Nothing else Just (x - 1, y)
    East  -> if x + 1 >= maxX then Nothing else Just (x + 1, y)
