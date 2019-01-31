--------------------------------------------------------------------------
-- Some experiments in bug's-eye view animation
-- 
-- Try it out.  For instance, try randPicker, (mousePicker lbp) or
-- (mousePicker updateDone), or (randThenMousePicker updateDone) as the
-- first argument and inhabs1, inhabs2, or inhabs3 as the second.
-- Choosing inhabs2 gives the fifteen puzzle, while inhabs3 is an
-- interesting variation.
--
-- Last modified Fri Nov 14 15:36:44 1997 by conal
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

-- A four-neighbor environment of booleans saying whether neighboring
-- spots are occupied
data MoverEnv  = MoverEnv { envWest, envEast, envNorth, envSouth :: Bool }
type MoverEnvB = Behavior MoverEnv

allFree = MoverEnv False False False False  -- for testing

type MoveRule = MoverEnv -> LocMove

moveESWN, neverMove :: MoveRule

-- Move into free neighbor space when asked.  Preferences: east, south,
-- west, north (ESWN).
moveESWN MoverEnv{ envEast  = False } = moveEast
moveESWN MoverEnv{ envSouth = False } = moveSouth
moveESWN MoverEnv{ envWest  = False } = moveWest
moveESWN MoverEnv{ envNorth = False } = moveNorth
moveESWN _                            = stayPut

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
data Inhabitant = Inhabitant Loc MoveRule (BoolB -> ImageB)

-- Move an inhabitant when told to by moveE.  (Should this yield an event
-- instead of a behavior?)
moveInhabitant :: Event MoverEnv -> Inhabitant -> LocB
moveInhabitant moveE (Inhabitant startLoc moveRule _) =
  stepper startLoc (scanlE addLocMove startLoc (moveE ==> moveRule))

renderInhabitant :: LocB -> Inhabitant -> ImageB
renderInhabitant locB inhab@(Inhabitant startLoc _ imGen) =
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
type World  = Array Loc Bool
type WorldB = Behavior  World

emptyWorld :: World
emptyWorld = array locRange [(loc,False) | loc <- allLocs]

-- Create a world from list of locations
fillWorld :: [Loc] -> World
fillWorld locs =
  -- Fill up an array, with False as the default, and (Just inhab) at
  -- loc if inhab is at loc.
  accumArray (\ _ () -> True) False locRange (zip locs (repeat ()))

fillWorldB :: [LocB] -> WorldB
fillWorldB = liftL fillWorld

-- Convert global information to local for consumption as needed by a move
-- rule.
neighbors :: World -> Loc -> MoverEnv
neighbors world loc =
  MoverEnv { envWest  = occupied moveWest
           , envEast  = occupied moveEast
           , envNorth = occupied moveNorth
           , envSouth = occupied moveSouth
           }
 where
   occupied :: LocMove -> Bool
   occupied locMove | validLoc loc' = world!loc'
                    | otherwise     = True
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
alwaysPicker :: LocMove -> DTime -> UPicker

-- Pick nothing
nothingPicker _ = neverE

alwaysPicker locMove dt u =
 uAlarm dt u -=> locMove

-- Pick locations at random
randPicker' u = randInt ==> toLoc
 where
   randInt = occsE (zip [t0, t0+0.2 ..] (randoms (floor (t0 * 10))))
   t0      = userStartTime u
   toLoc i = (i' `mod` cols, (i' `div` cols) `mod` rows)  where i' = abs i

-- Smarter algorithm: follows random path, moving one square at a time.
-- Once it chooses a moveable piece, it will keep on doing so.
randPicker u =
  updateDone u `withElemE_` randLocPath (floor (userStartTime u * 10))

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
uAlarm dt u = alarmE (userStartTime u) dt

-- Random until mouse motion sensed, and then mousePicker
randThenMousePicker button =
  randPicker `untilF` \u -> mouseMove u -=> mousePicker button

-- To do: one that goes back and forth between randPicker and mousePicker.
-- For this, we'll need to know mouseEnter and mouseExit events.


--------------------------------------------------------------------------
--                                 Demo
--------------------------------------------------------------------------

demo :: UPicker -> [Inhabitant] -> IO ()

demo upicker inhabitants = displayU imF
 where
   imF u = overs (zipWith renderInhabitant paths inhabitants)
    where
      -- Here use just the paths as their own obstacles, but we could add
      -- some other stuff as well.
      paths  = doMoves (upicker u) inhabitants worldB
      worldB = fillWorldB paths

main :: IO ()
main = demo (mousePicker updateDone) inhabs2

--------------------------------------------------------------------------
--                               Testing
--------------------------------------------------------------------------

-- Show a value, and color green if at home an red otherwise.
inhabIm :: Show a => a -> BoolB -> ImageB
inhabIm a = \ atHomeB -> withColor (condB atHomeB green red) aIm
 where
   aIm = stretch 2 (showIm a)

inhab1 = Inhabitant (0,0) neverMove (inhabIm 1)
inhab2 = Inhabitant (3,3) (alwaysMove (-1,-1)) (inhabIm 2)
inhab3 = Inhabitant (1,1) moveESWN (inhabIm 3)
inhab4 = Inhabitant (2,1) moveESWN (inhabIm 4)

inhabs1 = [inhab1,inhab2,inhab3,inhab4]

inhabs2 = [ Inhabitant startLoc moveESWN (startIm startLoc)
          | startLoc <- tail (range locRange)
          ]
 where
   startIm (col,row) = inhabIm (row * rows + col)

inhabs3 = tail (tail inhabs2)

-- Chop up a picture

cutUp pic = [ Inhabitant startLoc moveESWN (startIm startLoc)
            | startLoc <- tail (range locRange)
            ]
 where
   startIm loc = const (locCrop loc pic)

-- Sample pic's

rose  = stretch 0.7 (importBitmap "../Media/rose medium.bmp")
donut = stretch 2 $
        flipImage donutFlipBook (30 * 2 * pi * time / period)
 where
   period = 5

donutFlipBook :: HFlipBook
donutFlipBook = flipBook (bitmapDDSurface "../Media/donuts.bmp")
                         64 64 0 0 5 6

-- empty World
worldTest0 = fillWorld []

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
