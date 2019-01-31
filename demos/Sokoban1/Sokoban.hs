module Sokoban
       ( sokoban
       , levelOver
       , outside1
       , getFloorID
       ) where

import SokoType
import Fran
import List(findIndex)
import Array
import Win32(vK_LEFT, vK_UP, vK_RIGHT, vK_DOWN)

-----------------------------------------------------------------
-- sokoban
-----------------------------------------------------------------

sokoban :: Board -> World  -> Event Direction -> Behavior World
sokoban b w outside = worldB
  where
{- GSL: this is what I wanted but this should be avoided!
    sokoban' w = lift0 w `untilB` outside ==> \ d ->
                 if levelOver w b
                 then lift0 (final w)
                 else sokoban' (stir b d w)
-}

    worldB = stepper w happened

    happened :: Event World
    happened = outside `snapshot` worldB ==> uncurry (stir b)

-----------------------------------------------------------------
-- outside: outside stimulus
-----------------------------------------------------------------

outside1 :: User -> Event Direction
outside1 u = (   keyPress vK_UP    u -=> North
	     .|. keyPress vK_DOWN  u -=> South
 	     .|. keyPress vK_LEFT  u -=> West
 	     .|. keyPress vK_RIGHT u -=> East
 	     )

--  u `filterE` f
--   where
--     f (Key True c) | c == vK_UP    = Just North
-- 			 | c == vK_DOWN  = Just South
-- 			 | c == vK_LEFT  = Just West
-- 			 | c == vK_RIGHT = Just East
--     f _                            = Nothing

-----------------------------------------------------------------
-- stir: try to apply stimulus on pusher; if success, a new
-- world is returned; other the same old world is returned
-----------------------------------------------------------------

stir :: Board -> Direction -> World -> World
stir b d w =
  case stirAux 0 pusherForce w of
    Nothing -> w
    Just w' -> w'
  where
    -- try to stir Inhab i with Force f on World w
    stirAux :: ElemID -> Force -> World -> Maybe World
    stirAux i f w@(World inhabs) =
      let pos = getPos i inhabs		-- get my position on board
      in  getNeighborPos pos d >>= \ pos' -> -- may be out of bounds
          case getFloorID pos' b of	-- get neighbor's identity
            Wall      -> Nothing	-- can't move wall
            otherwise ->		-- empty/target/box
	      case getNeighborInhabID pos' inhabs of
    	        Nothing -> Just (simpleMove i pos' w) -- empty/target
    		Just i' ->		-- box
		  let weight = getWeight i inhabs -- get my weight
    		      f' = f - weight	-- compute residual force
    		  in  if f' <= 0
    		      then Nothing	-- run out of residual force
    		      else stirAux i' f' w >>= \ w' -> -- try stir neighbor
    		           Just (simpleMove i pos' w') -- go on to stir me

    simpleMove :: ElemID -> Pos -> World -> World
    simpleMove elemID pos (World inhabs) =
      let (inhabs1, (Inhab w _ : inhabs2)) = splitAt elemID inhabs
      in  World (inhabs1 ++ (Inhab w pos : inhabs2))

    getNeighborInhabID :: Pos -> [Inhab] -> Maybe ElemID
    getNeighborInhabID pos inhabs = findIndex f inhabs
      where f (Inhab _ pos') = pos == pos'

-----------------------------------------------------------------
-- levelOver: test if level is over
-----------------------------------------------------------------

levelOver :: Board -> Behavior World -> BoolB
levelOver b = lift1 f
  where
    f (World (_ : boxes)) = and (map isOnTarget boxes)

    isOnTarget :: Inhab -> Bool
    isOnTarget inhab =
      let p   = pos inhab
          fid = getFloorID p b
      in  case fid of
          Target -> True
    	  _      -> False
    
----------------------------------------------------------------
-- several utilities functions for access element in list and/or
-- array
----------------------------------------------------------------

pos :: Inhab -> Pos
pos (Inhab _ p) = p

getPos :: ElemID -> [Inhab] -> Pos
getPos elemID inhabs = pos (inhabs !! elemID)

getWeight :: ElemID -> [Inhab] -> Weight
getWeight elemID inhabs = let Inhab w _ = inhabs !! elemID in w

getFloorID :: Pos -> Board -> FloorID
getFloorID pos b = b ! pos

getNeighborPos :: Pos -> Direction -> Maybe Pos
getNeighborPos (x, y) d =
  case d of
    North -> if y + 1 >= maxY then Nothing else Just (x, y + 1)
    South -> if y - 1 < 0     then Nothing else Just (x, y - 1)
    West  -> if x - 1 < 0     then Nothing else Just (x - 1, y)
    East  -> if x + 1 >= maxX then Nothing else Just (x + 1, y)
