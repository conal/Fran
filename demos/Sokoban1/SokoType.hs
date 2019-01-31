module SokoType where

import Array

-----------------------------------------------------------------
-- World: type for configuration
-----------------------------------------------------------------

newtype World   = World [Inhab]
data    Inhab   = Inhab Weight Pos
type    ElemID  = Int			-- 0: pusher; n: box
data    FloorID = Empty | Target | Wall
type    Weight  = Int
type    Pos     = (Int, Int)
type    Board   = Array Pos FloorID

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