module SokoDemo where

import SokoType
import Sokoban
import SokoDraw
import "../Sokoban1/SokoBoard.hs" as SokoBoard
import Fran
import Array
import Win32(vK_ESCAPE)
import List(transpose)

-----------------------------------------------------------------
-- Instructions on run the examples.
--
-- To envoke individual levels, type "demo level?" where ?
-- currently is from 0 to 6.
--
-- To play all levels, type "main ()".
-----------------------------------------------------------------

main :: () -> IO ()
main _ = disp $ makeAll levels

demo :: (User -> (ImageB, BoolB)) -> IO ()
demo level = disp $ fst . level `untilF` \ u -> keyPress vK_ESCAPE u -=>
             const (stringIm "C H E A T E R !")

levels = [level0, level1, level2, level3, level4, level5, level6]

makeAll :: [User -> (ImageB, BoolB)] -> User -> ImageB
makeAll []     = const (stringIm "T H E    E N D")
makeAll (l:ls) = \ u ->
  stringIm "Loading  next  level  ..." `untilB`
  timeIs (startTime u ) -=>
  let (imgB, finalB) = l u in imgB `untilB`
  (keyPress vK_ESCAPE u .|.		-- ESC to next level
  predicate finalB u) `afterE_` u ==> makeAll ls

demo' :: [String] -> (User -> Event Direction) -> User -> (ImageB, BoolB)
demo' board outside u =
  let (b, locs) = config board
  in  toImageB b (sokoban b locs (outside u))

config :: [String] -> (Board, [Pos])
config s = (toBoard s, toLocs s)
  where
    flatString = concat $ transpose $ reverse s	-- reverse/transpose!

    toBoard s = listArray boundPos (map f flatString)
      where
        f c | c == ' '  = Empty
    	    | c == 'T'  = Target
    	    | c == 'W'  = Wall
    	    | c == 'P'  = Empty		-- pusher always on empty
    	    | c == 'B'  = Empty		-- box always on empty
	    | otherwise = Empty

    toLocs s = toLocsAux (zip (range boundPos) flatString)
                         (error "No Pusher") []
      where
        toLocsAux []            pusher boxes = pusher : boxes
        toLocsAux ((pos, c) : cs) pusher boxes =
	  case c of
	    'P' -> toLocsAux cs pos boxes
	    'B' -> toLocsAux cs pusher (pos : boxes)
	    _   -> toLocsAux cs pusher boxes


-----------------------------------------------------------------
-- demos: envoke by "demo level?" (level0 easiest)
-----------------------------------------------------------------

level0 = demo' board0 outside1
level1 = demo' board1 outside1
level2 = demo' board2 outside1
level3 = demo' board3 outside1
level4 = demo' board4 outside1
level5 = demo' board5 outside1
level6 = demo' board6 outside1
