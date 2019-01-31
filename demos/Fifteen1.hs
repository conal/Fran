--------------------------------------------------------------------------
-- Fifteen Puzzle demo.  Try "disp demo1", etc.
--
-- Shows mutually reactive behaviors
-- 
-- Last modified Fri Jul 25 14:32:23 1997
--------------------------------------------------------------------------

module Fifteen1 where

import Fran
import Random

--------------------------------------------------------------------------
-- The puzzle, given an event saying which piece to try moving
--------------------------------------------------------------------------

puzzle :: Event Loc -> ImageB

puzzle pickFrom =
  overs [movingPiece start (pieceLoc start) | start <- startPieceLocs]
 where
   -- Grab blank loc at pick.  Occurrences have (from,to)
   tryToMove  = pickFrom  `snapshot` blankLoc
   -- Only the valid ones
   moveFromTo = tryToMove `suchThat` adjacentLocs
   -- Separate From and To
   moveFrom = moveFromTo ==> fst
   moveTo   = moveFromTo ==> snd
   -- Location of the blank
   blankLoc = stepper startBlankLoc moveFrom
   -- A piece's location behavior based on start
   pieceLoc startLoc = myLoc
    where
      myLoc = stepper startLoc moveMeTo
      -- Look for legal moves from my location, yielding just the "to"
      moveMeTo = (moveFromTo `snapshot` myLoc) `filterE` \ ((from,to),me) ->
                 if (from == me) then Just to else Nothing
   -- All of the start locations
   startBlankLoc : startPieceLocs =
     [(i,j) | j <- [0 .. rows-1], i <- [0 .. cols-1]]
   -- A piece's appearance.  Show the piece number, colored green if in the
   -- home location and red otherwise, and moved to current location.
   movingPiece start@(col,row) locB =
     move (locToCenterPos locB .-. origin2)                $
     stretch 2                                             $
     withColor (cond (locB ==* constantB start) green red) $
     showIm (row * rows + col)


--------------------------------------------------------------------------
-- The "location" type.
--------------------------------------------------------------------------

type Loc  = (Int, Int)                  -- (column, row)
type LocB = Behavior Loc

rows, cols :: Int
rows = 4
cols = 4

puzzleWidth,  puzzleHeight :: RealB
(puzzleWidth, puzzleHeight) = (2, 2)

pieceWidth, pieceHeight :: RealB
pieceHeight = puzzleHeight / fromInt cols
pieceWidth  = puzzleWidth  / fromInt rows

pieceSize :: Vector2B
pieceSize     = vector2XY pieceWidth pieceHeight
pieceHalfSize = pieceSize ^/ 2

-- Map between locations and points.

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

-- Two locations are adjacent if one of the coordinates differs by one and
-- the other by zero.
adjacentLocs :: (Loc, Loc) -> Bool

adjacentLocs ((fromX, fromY), (toX, toY)) =
  abs (toX-fromX) + abs (toY-fromY) == 1


--------------------------------------------------------------------------
-- Various interfaces to the puzzle.  Each maps User input into a location
-- event.
--------------------------------------------------------------------------

mouseLocE, randLocE, randLocE', mouseThenRandLocE :: User -> Event Loc

-- Click on locations
mouseLocE u = lbp u `snapshot_` posToLoc (mouse u)

-- Sequence of random locations.
randLocE  u = occsE (zip times locs)
 where
   times = [t0, t0+0.1 ..]  where  t0 = startTime u
   locs = loop (userRandomInts u)
    where
      loop (i : j : ints') = (i `mod` 4, j `mod` 4) : loop ints'

-- randLocE is too random, since there are at most four legal moves at one
-- time, so more than three out of four tries get rejected.  This time,
-- keep track of the blank location, and add or subtract one from the row
-- or column each time.  Reject ones outside of the puzzle.

randLocE' u = occsE (zip times blankLocs)
 where
   times = [t0, t0+0.1 ..]  where  t0 = startTime u
   blankLocs = locsFrom (rows-1,0) (userRandomInts u)
    where
      locsFrom loc@(x,y) (i : j : bools')
        | newLocIsOk  =  newLoc : locsFrom newLoc bools'
        | otherwise   =  locsFrom loc bools'
       where
         newLoc@(x',y') | odd i     = (x, incOrDec j y)
                        | otherwise = (incOrDec j x, y)
         incOrDec n = if odd n then (+1) else (subtract 1)
         newLocIsOk = 0 <= x' && x' < cols &&
                      0 <= y' && y' < rows

-- Do random moves until a left button click and then play the game
mouseThenRandLocE u =
  randLocE' u `untilB` (lbp u `afterE_` u) ==> \ u' -> mouseLocE u'

-- Do random moves until a left button click and then play the game
mouseThenRandLocE' u =
  randLocE' u `untilB` (lbp u `afterE_` u) ==> \ u' -> mouseLocE u


--------------------------------------------------------------------------
-- The runnable demos.  Try "disp demo1", etc.
--------------------------------------------------------------------------

demo1, demo2, demo3, demo4, demo4' :: User -> ImageB

demo1  = puzzle . mouseLocE
demo2  = puzzle . randLocE
demo3  = puzzle . randLocE'
demo4  = puzzle . mouseThenRandLocE
demo4'  = puzzle . mouseThenRandLocE'


--------------------------------------------------------------------------
-- Misc utilities to move elsewhere
--------------------------------------------------------------------------

userRandomInts u = randomInts (floor (353 * t0)) (floor (173 * t0))
 where t0 = startTime u


{-

To do:

+ Aesthetic touches: add sound; highlighting pieces (if under mouse or
  legal to move or in the right place), etc.

+ Play with various puzzle images, including text and animations.
  Implement cropping.

+ Maybe simplify or eliminate the mapping between positions and locations.

+ An alternative for control: use arrow keys.  Each piece listens and
  moves in that direction if possible, but at most one piece will succeed.

-}
