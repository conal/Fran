--------------------------------------------------------------------------
-- Some experiments in bug's-eye view animation
--
-- Last modified Mon Jul 21 08:21:39 1997 by conal
--------------------------------------------------------------------------


module Mover where

import Fran
import Grid
import RandomIO

type Loc   = (Int, Int)                 -- Board location (column, row)
type LocB  = Behavior Loc

--------------------------------------------------------------------------
-- Move rules.  Each one only sees the availability status of four
-- neighboring locations and it yields just an incremental moved (dx,dy)
-- event.
--------------------------------------------------------------------------

data Space    = Free | Occupied
type SpaceB   = Behavior Space
data MoverEnv = MoverEnv Space Space Space Space -- left, right, below, above
type IncMove  = (Int, Int)                       -- (dx, dy) each -1, 0, or 1

type Mover = MoverEnv -> IncMove

moverRBLA, moverTest1 :: Mover

-- Move into free neighbor space when asked.  Preferences: right, below,
-- left, above (RBLA).
moverRBLA (MoverEnv _ Free _ _) = ( 1, 0)
moverRBLA (MoverEnv _ _ Free _) = ( 0,-1)
moverRBLA (MoverEnv Free _ _ _) = (-1, 0)
moverRBLA (MoverEnv _ _ _ Free) = ( 0, 1)
moverRBLA _                     = ( 0, 0)

-- Never move
moverTest1 _ = neverE           

-- Always make given IncMove
moverTest2 :: IncMove -> Mover
moverTest2 incMove _ = incMove          -- moverTest2 = const

--------------------------------------------------------------------------
--                             Inhabitants
--------------------------------------------------------------------------

data Inhabitant = Inhabitant Loc MoveRule ImageB              

--------------------------------------------------------------------------
--                          World-level stuff
--------------------------------------------------------------------------

type GridState  = Array Loc Space
type GridStateB = Behavior GridState

doMove :: Picker -> GridStateB -> Inhabitant -> LocB
doMove picker gridStateB (Inhabitant startLoc moveRule _) = locB
 where
   locB      = stepper startLoc moveTo
   moveTo    = scanlE startLoc (\(x,y) (dx,dy)->(x+dx, y+dy)) moveIncE
   moveIncE  = picker `snapshot_` moverEnvB
   moverEnvB = lift1 moveRule (neighborsB gridStateB locB)

neighbors :: GridState -> Loc -> MoverEnv
neighbors gridState (x,y) =
  MoverEnv (-1#0) (1#0) (0#-1) (0#1)
 where
   dx#dy  |  inRange loc'  =  gridState!loc'
          |  otherwise     =  Occupied
    where
      loc' = (x+dx,y+dy)

neighborsB :: GridStateB -> LocB -> MoverEnvB
neighborsB = lift2 neighbors


doMoves :: Picker -> [Inhabitant] -> [LocB]
doMoves picker inhabitants = paths
 where
   gridStateB = liftL fillGrid paths
   paths      = map (doMove picker gridStateB) inhabitants

fillGrid :: Loc -> GridState
fillGrid paths = mkArray fill locRange
 where
   fill loc | loc `elem` paths  =  Occupied
            | otherwise         =  Free
          

-- Define in Behavior.hs
liftL :: ([a] -> b) -> ([Behavior a] -> b)


-- [Move to Grid.hs]
locRange@(minLoc, maxLoc) = ((0,0),(cols-1,rows-1))

-- (From "A Gentle Introduction to Haskell")
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]


--------------------------------------------------------------------------
-- Various interfaces for telling a piece to move
--------------------------------------------------------------------------

type Picker  = Loc -> Event ()
type UPicker = User -> Picker

nothingPicker :: UPicker
alarmPicker   :: Duration -> UPicker
randPicker    :: UPicker
mousePicker   :: (User -> Event ()) -> UPicker

-- Pick nothing
nothingPicker u loc = neverE

-- Pick every location at regular intervals
alarmPicker dt u loc = alarm (startTime u) dt

-- Pick locations at random
randPicker u = randInt ==> toLoc
 where
   randInt = occsE (zip [t0, t0+0.2 ..] (randoms (floor (t0 * 10))))
   t0      = startTime u
   toLoc i = (i' `mod` cols, (i' `div` cols) `mod` rows)
             where i' = abs i

-- Locations selected by user.  For instance, use lbp or rbp for button,
-- but could use keypresses, an alarm, etc.
mousePicker button u loc =
  whenSnap (button u) (posToLoc (mouse u)) ( == loc)


--------------------------------------------------------------------------
-- Presentation
--------------------------------------------------------------------------

render :: Inhabitant -> LocB -> ImageB

render (Inhabitant _ _ imB) loc =
  move (locToCenterPos loc .-. origin2) imB


--------------------------------------------------------------------------
-- Put it together.
--------------------------------------------------------------------------

demo :: UPicker -> [Inhabitant] -> IO ()

demo upicker inhabitants = disp imF
 where
   imF u = overs (zipWith2 render inhabitants paths)
    where
      paths  = doMoves (upicker u) inhabitants


--------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------


whenSnap :: Event a -> Behavior b -> (a -> b -> Bool) -> Event a

whenSnap e b pred = (e `snapshot` b `suchThat` pred) ==> fst


--------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------


{- To do:

+ Move Loc and LocB to Grid.hs

+ Use inRange for validLoc.

+ Make events carry a start time, and re-implement startTime

-}
