-- Sokoban types.

module Types where
import Array
import Fran (Behavior, IntB, lift1)

-- Board location: (column, row)
-- Most of the Loc and Dir stuff is swiped from Fifteen.hs  Factor out
-- into a common module somewhere.
type Loc  = (Int,Int)
type LocB = Behavior Loc

cols, rows :: Num a => a
cols = 19; rows = 12

boundLoc :: (Loc, Loc)
boundLoc = ((0, 0), (cols - 1, rows - 1))

data Dir = East | West | North | South
  deriving (Enum,Show)

(+^) :: Loc -> Dir -> Loc
(col,row) +^ dir = case dir of
  West  -> (col-1, row  )
  East  -> (col+1, row  )
  North -> (col  , row-1)
  South -> (col  , row+1)

-- Level configuration
data Config = Config LevelFixed Loc [Loc]

-- Configuration for starting and save/load.  Level number, pusher
-- location, and box locations.  The permanent aspects of the level are
-- determined from the level number.
data SaveConfig = SaveConfig Int Loc [Loc]

-- Permanent features of a level.
type LevelFixed = Array Loc FixedId

-- A fixed feature is either blank, a target space or a wall
data FixedId = FixedBlank | FixedTarget | FixedWall deriving Eq

-- How many boxes are not on a target location.
boxesLeft :: Config -> [LocB] -> IntB
boxesLeft (Config fixed _ _) boxLocs =
   sum (map (lift1 offTarget) boxLocs)
 where
   offTarget :: Loc -> Int
   offTarget loc | fixed ! loc == FixedTarget = 0
                 | otherwise                  = 1
