module Play where

import Array
import Types
import Fran


-- Abstract version of playing one level.

playLevel :: Config -> Event Dir -> (LocB, [LocB],IntB)
playLevel config@(Config fixed pusher0 boxes0) requestDir =
  (pusherLoc, boxLocs, left)
 where
   -- Whether loc has a box.  Changes when a box moves to or from loc.
   hasBox :: Array Loc BoolB
   hasBox = fArray boundLoc $ \ loc ->
            (loc `elem` boxes0) `stepper`
              (boxEnter!loc -=> True .|. boxLeave!loc -=> False)

   boxLeave :: Array Loc (Event Loc)    -- to where
   boxLeave = arrayE boundLoc boxFromTo

   boxEnter :: Array Loc (Event ())
   boxEnter = arrayE boundLoc (boxFromTo ==> snd ==> (`pair` ()))

   -- A box moves when the pusher moves to a location with a box
   boxFromTo :: Event (Loc,Loc)
   boxFromTo =
     whenEP (movePusher ==> \ (dir, pTo) -> ((pTo, pTo+^dir), hasBox!pTo))
     `traceE` "boxFromTo"

   -- A pusher can move if either the destination space is unoccupied or
   -- if it contains a box that can move in the requested direction.  A
   -- box can move if either the destination space is unoccupied.
   movePusher :: Event (Dir,Loc)        -- pusher dir,to
   movePusher =
     whenEP (requestDir `snapshot` pusherLoc ==> \(dir,pLoc) ->
              let pTo = pLoc+^dir in
              ( (dir, pTo)
              , isFree pTo ||* (hasBox!pTo &&* isFree (pTo+^dir))))
     `traceE` "movePusher"

   pusherLoc :: LocB
   pusherLoc = pusher0 `stepper` movePusher ==> snd

   boxLocs :: [LocB]
   boxLocs = map boxLoc boxes0

   boxLoc :: Loc -> LocB
   boxLoc loc0 = loc  where  loc = loc0 `stepper` boxLeaveAt loc

   boxLeaveAt :: LocB -> Event Loc
   boxLeaveAt = (boxLeave !*)

   isFree :: Loc -> BoolB
   isFree loc | fixed!loc == FixedWall  =  falseB
              | otherwise               =  notB (hasBox!loc)

   left :: IntB
   left = boxesLeft config boxLocs


----- Misc utilities.

fArray :: Ix ix => (ix,ix) -> (ix -> b) -> Array ix b
fArray bounds f = array bounds [(ix, f ix) | ix <- range bounds]

