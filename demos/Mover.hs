--------------------------------------------------------------------------
-- Some experiments in bug's-eye view animation
--
-- Last modified Thu Aug 07 12:50:08 1997 by conal
--------------------------------------------------------------------------


module Mover where

import Fran
import List
import Array
import Grid
import RandomIO

import Trace -- while debugging

--------------------------------------------------------------------------
-- Move rules.  Each one only sees the contents of four neighboring
-- locations and it yields just an incremental moved (dx,dy) event.
--------------------------------------------------------------------------

-- A four-neighbor environment of possible values.
data MoverEnv  = MoverEnv { envWest, envEast, envNorth, envSouth
                            :: Maybe Inhabitant }
type MoverEnvB = Behavior MoverEnv

allFree = MoverEnv Nothing Nothing Nothing Nothing  -- for testing

type MoveRule = MoverEnv -> LocMove

moveESWN, neverMove :: MoveRule

-- Move into free neighbor space when asked.  Preferences: east, south,
-- west, north (ESWN).
moveESWN MoverEnv{ envEast  = Nothing } = moveEast
moveESWN MoverEnv{ envSouth = Nothing } = moveSouth
moveESWN MoverEnv{ envWest  = Nothing } = moveWest
moveESWN MoverEnv{ envNorth = Nothing } = moveNorth
moveESWN _                              = stayPut

-- Never move
neverMove _ = stayPut

-- Always make given LocMove
alwaysMove :: LocMove -> MoveRule
alwaysMove locMove _ = locMove          -- alwaysMove = const

{-
traceMoveRule :: String -> MoveRule -> MoveRule
traceMoveRule str rule env =
  traceShow ("Move rule: " ++ str ++ " " ++ show env ++ " == ") (rule env)
-}

--------------------------------------------------------------------------
--                             Inhabitants
--------------------------------------------------------------------------

-- An inhabitant is defined by a start, a move rule, and an imaging
-- function that is told whether it is in the right place.
data Inhabitant = Mover Loc MoveRule (BoolB -> ImageB)
                | Wall Loc

-- This guy represents a wall.  It really deserves another constructor in
-- the Inhabitant type.
wallInhab :: Loc -> Inhabitant
wallInhab = Wall

-- Move an inhabitant when told to by moveE.  (Should this yield an event
-- instead of a behavior?)
moveInhabitant :: Event MoverEnv -> Inhabitant -> LocB
moveInhabitant moveE (Mover startLoc moveRule _) =
  stepper startLoc (scanlE addLocMove startLoc (moveE ==> moveRule))

moveInhabitant moveE (Wall loc) = error "Can't move a wall!"

renderInhabitant :: LocB -> Inhabitant -> ImageB
renderInhabitant locB inhab@(Mover startLoc _ imGen) =
  move (locToMotionB locB)
       (imGen (locB ==* constantB startLoc))


--------------------------------------------------------------------------
--                          World-level stuff
--
-- In contrast to move rules, which use only local perception and produce
-- local motion, a "world" manages global information.  It does
-- global-to-local conversion and filtering for each inhabitant, and then
-- turns the inhabitants' local actions into global terms, to be fed back
-- to all inhabitants.  Thus inhabitants and worlds play symbiotic
-- relationships, as reflected formally in mutually recursions.
--------------------------------------------------------------------------

-- A grid-shaped world
type World  = Array Loc (Maybe Inhabitant)
type WorldB = Behavior  World

emptyWorld :: World
emptyWorld = array locRange [(loc,Nothing) | loc <- allLocs]

-- Create a world from list of locations
fillWorld :: [Inhabitant] -> [Loc] -> World
fillWorld inhabs locs =
  -- Fill up an array, with Nothing as the default, and (Just inhab) at
  -- loc if inhab is at loc.
  accumArray (\ _ inhab -> Just inhab) Nothing locRange (zip locs inhabs)

fillWorldB :: [Inhabitant] -> [LocB] -> WorldB
fillWorldB inhabs = liftL (fillWorld inhabs)

-- Convert global information to local for consumption as needed by a move
-- rule.
neighbors :: World -> Loc -> MoverEnv
neighbors world loc =
  MoverEnv { envWest  = what moveWest
           , envEast  = what moveEast
           , envNorth = what moveNorth
           , envSouth = what moveSouth
           }
 where
   -- what :: LocMove -> Maybe a
   what locMove | validLoc loc' = world!loc'
                | otherwise     = Just (wallInhab loc')
    where
      loc' = addLocMove loc locMove

neighborsB :: WorldB -> LocB -> MoverEnvB
neighborsB = lift2 neighbors


-- Move an Inhabitantround in a changing world
doMove :: Picker -> WorldB -> Inhabitant -> LocB
doMove picker worldB inhabitant = locB
 where
   locB = moveInhabitant envE inhabitant
   envE = pickThis `snapshot_` neighborsB worldB locB
   pickThis = whenSnap picker locB (==)

-- Move a collection of inhabitants, given some obstacles (some or all of
-- which may turn out to be the inhabitants themselves).
doMoves :: Picker -> [Inhabitant] -> WorldB -> [LocB]
doMoves picker inhabitants worldB =
  map (doMove picker worldB) inhabitants

--------------------------------------------------------------------------
-- Various interfaces for telling a piece to move
--------------------------------------------------------------------------

-- Pick a location.
type Picker  = Event Loc

-- Pick a location based on the world and the user
type UPicker = User -> Picker

nothingPicker, randPicker, randPicker' :: UPicker
mousePicker, randThenMousePicker :: (User -> Event a) -> UPicker

-- Pick nothing
nothingPicker _ = neverE

-- Pick locations at random
randPicker' u = randInt ==> toLoc
 where
   randInt = occsE (zip [t0, t0+0.2 ..] (randoms (floor (t0 * 10))))
   t0      = startTime u
   toLoc i = (i' `mod` cols, (i' `div` cols) `mod` rows)  where i' = abs i

-- Smarter algorithm: follows random path, moving one square at a time.
-- Once it chooses a moveable piece, it will keep on doing so.
randPicker u =
  updateDone u `withElemE_` randLocPath (floor (startTime u * 10))

randLocPath :: Int -> [Loc]
randLocPath seed = path minLoc (map toLocMove (randoms seed))
 where
   path loc locMoves = loc : more locMoves
    where
      -- Make implied move if valid
      more (locMove : locMoves') | validLoc loc' = path loc' locMoves'
                                 | otherwise     = more locMoves'
       where
         loc' = addLocMove loc locMove
   toLocMove i = case i `mod` 4 of
                   0 -> ( 1, 0)
                   1 -> ( 0, 1)
                   2 -> (-1, 0)
                   3 -> ( 0,-1)

-- Locations selected by user.  For instance, use lbp or rbp for button,
-- but could use keypresses, uAlarm below, etc.
mousePicker button u = button u `snapshot_` motionToLocB (mouseMotion u)

uAlarm :: DTime -> User -> Event ()
uAlarm dt u = alarmE (startTime u) dt

-- Random until mouse motion sensed, and then mousePicker
randThenMousePicker button =
  randPicker `untilF` \u -> mouseMove u -=> mousePicker button

-- To do: one that goes back and forth between randPicker and mousePicker.
-- For this, we'll need to know mouseEnter and mouseExit events.


--------------------------------------------------------------------------
-- Try it out.  For instance, try randPicker, (mousePicker lbp) or
-- (mousePicker updateDone), or (randThenMousePicker updateDone) as the
-- first argument and inhabs1, inhabs2, or inhabs3 as the second.
-- Choosing inhabs2 gives the fifteen puzzle, while inhabs3 is an
-- interesting variation.
--------------------------------------------------------------------------

demo :: UPicker -> [Inhabitant] -> IO ()

demo upicker inhabitants = disp imF
 where
   imF u = overs (zipWith renderInhabitant paths inhabitants)
    where
      -- Here use just the paths as their own obstacles, but we could add
      -- some other stuff as well.
      paths  = doMoves (upicker u) inhabitants worldB
      worldB = fillWorldB inhabitants paths



--------------------------------------------------------------------------
--                               Testing
--------------------------------------------------------------------------

-- Show a value, and color green if at home an red otherwise.
inhabIm :: Show a => a -> BoolB -> ImageB
inhabIm a = \ atHomeB -> withColor (cond atHomeB green red) aIm
 where
   aIm = stretch 2 (showIm a)

inhab1 = Mover (0,0) neverMove (inhabIm 1)
inhab2 = Mover (3,3) (alwaysMove (-1,-1)) (inhabIm 2)
inhab3 = Mover (1,1) moveESWN (inhabIm 3)
inhab4 = Mover (2,1) moveESWN (inhabIm 4)

inhabs1 = [inhab1,inhab2,inhab3,inhab4]

inhabs2 = [ Mover startLoc moveESWN (startIm startLoc)
          | startLoc <- tail (range locRange)
          ]
 where
   startIm (col,row) = inhabIm (row * rows + col)

inhabs3 = tail (tail inhabs2)

{-
moverTest :: Inhabitant -> User -> ImageB
moverTest inhab u = renderInhabitant loc inhab
 where
   loc   = moveInhabitant moveE inhab
   moveE = lbp u -=> allFree
   -- moveE = alarmE (startTime u) 1 -=> allFree

-- empty World
worldTest0 = fillWorld []
-- Diagonally occupied World
worldTest1 = fillWorld [ (i,i) | i <- [0 .. (cols `min` rows) - 1] ]

-- Tester for doMove
doMoveTest inhab upicker world u = renderInhabitant loc inhab
 where
   loc = doMove (upicker u) (constantB world) inhab
-}

--------------------------------------------------------------------------
--                                To do
--------------------------------------------------------------------------
{- 

+ A mystery: (mousePicker $ uAlarm 0.1) space leaks and slows down, while
  randPicker doesn't seem to.  Even worse is (mousePicker updateDone).
  What's going on???  Note that mouse and updateDone are defined similarly
  in Interaction.hs.  However, the patMouse example in src\Spritify.hs
  doesn't have this problem.

+ Make the puzzle know when it's done.

+ Add rectangular trimming to ImageB and use for making a photo background.

+ Make some more interesting movers.  They might collect things along the
  way, or exhibit pursuit/fleeing behavior.

+ Smooth movement in moveInhabitant.

+ Mover with memory.  For instance, make movers that tend to spiral.

+ Arrow key interface.  This could be neat, but seems to violate the
  MoveRule interface, which lets the mover decide the destination.  Could
  widen the interface, e.g., Picker = Event (Maybe LocMove), where a
  Nothing means move wherever, while a Just means try to move in the given
  direction.

-}
