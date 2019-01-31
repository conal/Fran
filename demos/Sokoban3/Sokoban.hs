module Sokoban where

import Fran
import SokoType
import Array
import Maybe(fromJust)
import Win32(vK_LEFT, vK_UP, vK_RIGHT, vK_DOWN)

-----------------------------------------------------------------
-- sokoban
-----------------------------------------------------------------

sokoban :: Board -> [Pos] -> Event Direction -> ([Behavior Pos], BoolB)
sokoban board initLocs outside = (locBs, finalB)
  where
    locBs = lazyZipWith doMoves initLocs moveInhabEs

    -------------------------------------------------------------
    -- very important: lazy in second list!
    -------------------------------------------------------------

    lazyZipWith f [] _ = []
    lazyZipWith f (a:as) ~(b:bs) = f a b : lazyZipWith f as bs

    behOfList = liftL id locBs

    moveInhabEs :: [Event Direction]
    moveInhabEs = map (\ locB -> whenSnap moveLocsE locB
					     (\ (_, moved) loc ->
					     loc `elem` moved)
				    ==> fst)
		      locBs

    moveLocsE :: Event (Direction, [Pos])
    moveLocsE = (outside `snapshot` behOfList) ==> \ (d, locs) ->
		(d, moveableLocs d locs)

    moveableLocs :: Direction -> [Pos] -> [Pos]
    moveableLocs d (pusher : boxes) =
      loop pusherForce [pusher] (getNeighborPos pusher d)
      where
        loop forceLeft soFar Nothing  = [] -- out of bounds
	loop forceLeft soFar (Just p) =
	  if board ! p == Wall then []
	  else if p `elem` boxes	-- box
	       then if forceLeft <= 0 then []
	            else loop (forceLeft - boxWeight)
			      (p : soFar)
			      (getNeighborPos p d)
	       else soFar		-- empty

    doMoves :: Pos -> Event Direction -> Behavior Pos
    doMoves p ev = posB
       where
         posB  = stepper p evPos
	 evPos = ev `snapshot` posB ==> \ (d, p') ->
		 fromJust $ getNeighborPos p' d

    finalB :: BoolB
    finalB = lift1 levelOver behOfList
      where
        levelOver :: [Pos] -> Bool
	levelOver (_ : boxes) = and $ map (\ p -> board ! p == Target) boxes

-----------------------------------------------------------------
-- outside
-----------------------------------------------------------------

outside1 :: User -> Event Direction
outside1 u = (   keyPress vK_UP    u -=> North
	     .|. keyPress vK_DOWN  u -=> South
 	     .|. keyPress vK_LEFT  u -=> West
 	     .|. keyPress vK_RIGHT u -=> East
 	     )
