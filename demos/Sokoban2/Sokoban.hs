-----------------------------------------------------------------
-- Last modified Mon Oct 06 17:16:55 1997 by t-garyl
--
-- This approach does not quite work in that the computing element
-- hang on to all the old data (event occurrence) and when doing
-- event detection, they get the wrong/old occurrence.
--
-- Also, rebuilding the whole world every time is a very heavy
-- overhead and so even displaying the wrong data, the ups is
-- close to 1.5.
--
-----------------------------------------------------------------

module Sokoban where

import SokoType
import Fran
import Array
import Maybe(fromJust)
import List(find)
import Win32(vK_LEFT, vK_UP, vK_RIGHT, vK_DOWN)

-----------------------------------------------------------------
-- sokoban
-----------------------------------------------------------------

sokoban :: Floor -> [Inhab] -> Event Direction -> ([InhabB], Board, BoolB)
sokoban floor initInhabs ev = (inhabBs, board, finalB)
  where

    -------------------------------------------------------------
    -- potentialPush
    -------------------------------------------------------------
    
    push :: Event Direction -> Push
    push e = array boundPos [ (p, f p) | p <- range boundPos ]
      where
    	f :: Pos -> DirArray (Event Force)
    	f p = array boundDir [ (d, ff p d) | d <- range boundDir ]
    
    	ff :: Pos -> Direction -> Event Force
    	ff p d =
    	  let outsideEv =
    		(e `snapshot` (board ! p)) `filterE` \ (outsideDir, me) ->
    		if (isPusher me)
		then if (d == outsideDir)
		     then Just pusherForce
		     else Just 0
		else if isBox me then Nothing
		     else Just 0	-- empty
    
    	      neighborEv =
    		case getNeighborPos p (opposite d) of
    		  Nothing -> neverE	-- out of bounds
    		  Just p' ->		-- neighbor's position
    		    ((push e ! p') ! d) `snapshot` (board ! p')
    		    `filterE` \ (neighborForce, neighbor) ->
    		    let residual = neighborForce - getWeight neighbor
    		    in  if neighborForce <= 0 then Nothing
		        else if residual > 0 then Just residual else Nothing
    	  in  outsideEv .|. neighborEv
    
    -------------------------------------------------------------
    -- resist
    -------------------------------------------------------------
    
    resist :: Resist
    resist = array boundPos [ (p, f p) | p <- range boundPos ]
      where
    	f :: Pos -> DirArray (Behavior Force)
    	f p = array boundDir [ (d, ff p d) | d <- range boundDir ]
    
    	ff :: Pos -> Direction -> Behavior Force
    	ff p d =
    	  let meB = board ! p
    
    	      nonWallB =
    		condB (isPusherB meB ||* -- pusher
    		       isBoxB meB)	-- box
    		      pusherOrBoxB
    		      0			-- empty
    
    	      pusherOrBoxB =
    		case getNeighborPos p d of
    		  Nothing -> infinityB	-- out of bounds
    		  Just p' -> getWeightB meB `addForceB`
    			     ((resist ! p') ! d)
    	  in  if floor ! p == Wall then infinityB else nonWallB
    
    -------------------------------------------------------------
    -- board: if using ev to snapshot inhabs, this may cause
    --        inconsistency among inhabitants because their
    --        values are needed at different stage of the whole
    --        computation.
    -------------------------------------------------------------
    
    board :: Board
    board = array boundPos [ (p, f p) | p <- range boundPos ]
      where
        f :: Pos -> Behavior Element
	f p = lift1 (f' p) behListInhab

        f' :: Pos -> [Inhab] -> Element
        f' p inhabs =
	  case find (\ (_, (w, pos)) -> pos == p) (zip [0 ..] inhabs) of
	    Nothing          -> (Nothing, False)
	    Just (i, (w, _)) -> (Just w, i == 0)

    -------------------------------------------------------------
    -- Behavior [Inhab]
    -------------------------------------------------------------

    behListInhab :: Behavior [Inhab]
    behListInhab = toBehListInhab inhabBs

    toBehListInhab :: [InhabB] -> Behavior [Inhab]
    toBehListInhab inhabBs =
      let posBs = map getPosB inhabBs
          weights = map getInhabWeightB inhabBs
	  realInhabBs = zipWith (lift1 . f) weights posBs

	  f :: Weight -> Pos -> Inhab
	  f w pos = (w, pos)
      in  liftL id realInhabBs                   

    -------------------------------------------------------------
    -- inhabBs
    -------------------------------------------------------------
    
    inhabBs :: [InhabB]
    inhabBs = map f initInhabs
      where
    	f (w, p) = (w, posB p ev)

        posB p e =
	  lift0 p `untilB` anyE (map (trigger p e) (range boundDir))
	  `afterE` e ==> uncurry posB

    	trigger :: Pos -> Event Direction -> Direction -> Event Pos
    	trigger p e d =
    	  let evPush = (push e ! p) ! d
    	      resistB = (resist ! p) ! d
    	  in  (evPush `snapshot` resistB `suchThat` \ (p, r) ->
	      p > 0 && p >= r) -=> fromJust (getNeighborPos p d)

    -------------------------------------------------------------
    -- finalB
    -------------------------------------------------------------

    finalB :: BoolB
    finalB = lift1 final behListInhab
      where
        final :: [Inhab] -> Bool
	final inhabs = and $ map f (tail inhabs) -- tail are boxes

        f :: Inhab -> Bool
	f (_, pos) = floor ! pos == Target
	
-----------------------------------------------------------------
-- outside
-----------------------------------------------------------------

outside1 :: User -> Event Direction
outside1 u = (   keyPress vK_UP    u -=> North
	     .|. keyPress vK_DOWN  u -=> South
 	     .|. keyPress vK_LEFT  u -=> West
 	     .|. keyPress vK_RIGHT u -=> East
 	     )
