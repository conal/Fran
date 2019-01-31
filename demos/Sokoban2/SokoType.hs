module SokoType where

import Fran
import Array(Array(..))
import Maybe(isJust)

-----------------------------------------------------------------
-- Arrays
-----------------------------------------------------------------

type PosArray a = Array Pos a
type DirArray a = Array Direction a

type Board  = PosArray (Behavior Element)
type Resist = PosArray (DirArray (Behavior Force))
type Push   = PosArray (DirArray (Event Force))
type Floor  = PosArray FloorID

-----------------------------------------------------------------
-- 2 index types
-----------------------------------------------------------------

type Pos = (Int, Int)

maxX, maxY :: Int
maxX = 19
maxY = 12

boundPos :: (Pos, Pos)
boundPos = ((0, 0), (maxX - 1, maxY - 1))

data Direction = North | East | South | West deriving (Show,Eq,Ord,Ix)

boundDir :: (Direction, Direction)
boundDir = (North, West)

opposite :: Direction -> Direction
opposite d | d == North = South
           | d == East  = West
	   | d == South = North
	   | d == West  = East

-----------------------------------------------------------------
-- Inhab
-----------------------------------------------------------------

type Inhab  = (Weight, Pos)
type InhabB = (Weight, Behavior Pos)

getInhabWeight :: Inhab -> Weight
getInhabWeight = fst

getInhabWeightB :: InhabB -> Weight
getInhabWeightB = fst

getPos :: Inhab -> Pos
getPos  = snd

getPosB :: InhabB -> Behavior Pos
getPosB = snd

getNeighborPos :: Pos -> Direction -> Maybe Pos
getNeighborPos (x, y) d =
  case d of
    North -> if y + 1 >= maxY then Nothing else Just (x, y + 1)
    South -> if y - 1 < 0     then Nothing else Just (x, y - 1)
    West  -> if x - 1 < 0     then Nothing else Just (x - 1, y)
    East  -> if x + 1 >= maxX then Nothing else Just (x + 1, y)

-----------------------------------------------------------------
-- misc
-----------------------------------------------------------------

type Element = ( Maybe Weight		-- weight for pusher/box
               , Bool			-- is pusher?
	       )

data FloorID = Empty | Target | Wall deriving (Show,Eq)

isPusher :: Element -> Bool
isPusher (_, pusher) = pusher

isBox :: Element -> Bool
isBox (mbWeight, pusher) = isJust mbWeight && not pusher

getWeight :: Element -> Weight
getWeight (Just weight, _) = weight
getWeight _                = error "internal error from getWeight"

isPusherB, isBoxB :: Behavior Element -> BoolB
isPusherB = lift1 isPusher
isBoxB    = lift1 isBox

getWeightB :: Behavior Element -> Behavior Weight
getWeightB = lift1 getWeight

-----------------------------------------------------------------
-- force
-----------------------------------------------------------------

type Weight = Int
type Force  = Weight

infinity :: Force
infinity = 10000

infinityB :: Behavior Force
infinityB = lift0 infinity

pusherForce :: Force
pusherForce = 1

addForce :: Force -> Force -> Force
addForce f1 f2 = infinity `min` (f1 + f2)

addForceB :: Behavior Force -> Behavior Force -> Behavior Force
addForceB = lift2 addForce

pusherWeight, boxWeight :: Weight
pusherWeight = 0
boxWeight = 1