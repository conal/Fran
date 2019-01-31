-- The classic "fifteen puzzle".  This module describes just the abstract
-- game, without concrete input/output.

module Puzzle where

import Fran
import Grid

-- Action of a single piece, given (a) move request containing move
-- direction and blank location, and (b) where to start.  Yields the
-- piece's location and a move event.
goPiece :: Event (Dir, Loc) -> Loc -> (LocB, Event ())
goPiece moveReq loc0 = (locB, moveTo -=> ())
 where
   locB = stepper loc0 moveTo
   -- Move when requested if the blank is in the given direction
   moveTo = moveReq `snapshot` locB
                    `filterE`  filt

   filt ((dir, bLoc), loc)
     | loc `addDir` dir == bLoc  =  Just bLoc
     | otherwise                 =  Nothing


-- Action of all pieces, given directed move attempts.
goPieces :: Event Dir -> [LocB]
goPieces moveDir = locBs
 where
   -- Track locations and moves of all pieces
   (locBs, moves) = unzip (map (goPiece moveReq) loc0s)
   -- The blank space moves to the location vacated by a moving piece
   blankLocB = stepper blankLoc0 (anyE moveFroms)
   -- Snapshot of each pieces's location just before it moves
   moveFroms = zipWith snapshot_ moves locBs

   moveReq = moveDir `snapshot` blankLocB
