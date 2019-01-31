--------------------------------------------------------------------------
-- Some experiments in bug's-eye view animation
--
-- Last modified Wed Jul 23 12:04:21 1997 by conal
--------------------------------------------------------------------------


module Mover where

import Fran
import List
import Array
import Grid
import RandomIO

import Trace -- while debugging

--------------------------------------------------------------------------
-- Move rules.  Each one only sees the availability status of four
-- neighboring locations and it yields just an incremental moved (dx,dy)
-- event.
--------------------------------------------------------------------------

data Space     = Free | Occupied  deriving Show
type SpaceB    = Behavior Space
data MoverEnv = MoverEnv{ envWest, envEast, envNorth, envSouth :: Space }
  deriving Show
type MoverEnvB = Behavior MoverEnv

allFree = MoverEnv Free Free Free Free  -- for testing

type MoveRule  = MoverEnv -> LocMove

moveESWN, neverMove :: MoveRule

-- Move into free neighbor space when asked.  Preferences: east, south,
-- west, north (ESWN).
moveESWN MoverEnv{ envEast  = Free } = moveEast
moveESWN MoverEnv{ envSouth = Free } = moveSouth
moveESWN MoverEnv{ envWest  = Free } = moveWest
moveESWN MoverEnv{ envNorth = Free } = moveNorth
moveESWN _                           = stayHere

-- Never move
neverMove _ = stayHere

-- Always make given LocMove
alwaysMove :: LocMove -> MoveRule
alwaysMove locMove _ = locMove          -- alwaysMove = const

traceMoveRule :: String -> MoveRule -> MoveRule
traceMoveRule str rule env =
  traceShow ("Move rule: " ++ str ++ " " ++ show env ++ " == ") (rule env)

--------------------------------------------------------------------------
--                             Inhabitants
--------------------------------------------------------------------------

data Inhabitant = Inhabitant Loc MoveRule ImageB              

-- Move an inhabitant when told to by moveE.  (Should this yield an event
-- instead of a behavior?)
moveInhabitant :: Event MoverEnv -> Inhabitant -> LocB
moveInhabitant moveE (Inhabitant startLoc moveRule _) =
  stepper startLoc (scanlE addLocMove startLoc (moveE ==> moveRule))

-- Render a moved inhabitant
renderInhabitant :: LocB -> Inhabitant -> ImageB
renderInhabitant loc (Inhabitant _ _ imB) =
  move (locToCenterPos loc .-. origin2) imB

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
type World  = Array Loc Space
type WorldB = Behavior World

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
   what :: LocMove -> Space
   what locMove | validLoc loc' = world!loc'
                | otherwise     = Occupied
    where
      loc' = addLocMove loc locMove

neighborsB :: WorldB -> LocB -> MoverEnvB
neighborsB = lift2 neighbors

-- Move an inhabitant around in a changing world
doMove :: Picker -> WorldB -> Inhabitant -> LocB
doMove picker worldB inhabitant = locB
 where
   locB = moveInhabitant envE inhabitant
   envE = picker locB `snapshot_` neighborsB worldB locB

-- Move a collection of inhabitants, given some obstacles (some or all of
-- which may turn out to be the inhabitants themselves).
doMoves :: Picker -> [Inhabitant] -> [LocB] -> [LocB]
doMoves picker inhabitants obstacles = paths
 where
   worldB = fillWorldB obstacles
   paths  = map (doMove picker worldB) inhabitants

-- Create a world
fillWorld :: [Loc] -> World
fillWorld locs = mkArray fill locRange
 where
   fill loc | loc `elem` locs  =  Occupied
            | otherwise        =  Free

fillWorldB :: [LocB] -> WorldB
fillWorldB = liftL fillWorld          

--------------------------------------------------------------------------
-- Various interfaces for telling a piece to move
--------------------------------------------------------------------------

type Picker  = LocB -> Event ()
type UPicker = User -> Picker

nothingPicker :: UPicker
alarmPicker   :: Duration -> UPicker
randPicker    :: UPicker
buttonPicker, mousePicker :: (User -> Event ()) -> UPicker

-- Pick nothing
nothingPicker u loc = neverE

-- Pick every location at regular intervals
alarmPicker dt u loc = alarmE (startTime u) dt

-- Pick locations at random
randPicker u locB =
  -- When a random location is equal to the given locB
  whenSnap (randInt ==> toLoc) locB (==) -=>  ()
 where
   randInt = occsE (zip [t0, t0+0.2 ..] (randoms (floor (t0 * 10))))
   t0      = startTime u
   toLoc i = (i' `mod` cols, (i' `div` cols) `mod` rows)
             where i' = abs i

-- Picks everthing when the button is pressed
buttonPicker button u locB = button u

-- Locations selected by user.  For instance, use lbp or rbp for button,
-- but could use keypresses, an alarm, etc.
mousePicker button u locB =
  buttonPicker button u locB `whenE` (posToLoc (mouse u) ==* locB)


--------------------------------------------------------------------------
-- Try it out.  For instance, try (mousePicker lbp) or (alarmPicker 0.1)
-- as the first argument and inhabs1 or inhabs2 as the second.  Choosing
-- inhabs2 gives the fifteen puzzle, and inhabs3 is an interesting
-- variation.  Using (alarmPicker 1) shows that the inhabitants can pile
-- up, since they will move to the same free space at the same time.
--------------------------------------------------------------------------

demo :: UPicker -> [Inhabitant] -> IO ()

demo upicker inhabitants = disp imF
 where
   imF u = overs (zipWith renderInhabitant paths inhabitants)
    where
      -- Here use just the paths as their own obstacles, but we could add
      -- some other stuff as well.
      paths = doMoves (upicker u) inhabitants paths


--------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------

-- Move to BehaviorEvent.hs, and use in whenE's definition.
whenSnap :: Event a -> Behavior b -> (a -> b -> Bool) -> Event a
whenSnap e b pred = (e `snapshot` b `suchThat` uncurry pred) ==> fst

-- Define in Behavior.hs
liftL :: ([a] -> b) -> ([Behavior a] -> Behavior b)

-- Convert [Behavior a] to Behavior [a] and use lift1
liftL f bs = lift1 f (foldr consB nilB bs)

-- (From "A Gentle Introduction to Haskell")
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]

type Duration = Time

--------------------------------------------------------------------------
--                               Testing
--------------------------------------------------------------------------

inhabIm :: Show a => a -> ImageB
inhabIm = stretch 2 . withColor red . showIm

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


--------------------------------------------------------------------------
--                                To do
--------------------------------------------------------------------------
{- 

+ Smooth movement in moveInhabitant.

+ Mover with memory.  For instance, make movers that tend to spiral.

+ Arrow key interface.

-}
