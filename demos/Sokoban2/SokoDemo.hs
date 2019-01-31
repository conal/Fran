module SokoDemo where

import SokoType
import Sokoban
import SokoDraw
import "../Sokoban1/SokoBoard" as SokoBoard
import Fran
import List(transpose)
import Array

-----------------------------------------------------------------
-- demo: due to its inefficiency, try level by level as
--       disp $ demo board?
-- where ? ranges from 0 to 6
-----------------------------------------------------------------

demo :: [String] -> User -> ImageB
demo s = fst . (demo' s outside1)

demo' :: [String] -> (User -> Event Direction) -> User -> (ImageB, BoolB)
demo' s outside u =
  let (floor, initInhabs) = config s
  in  toImageB floor (sokoban floor initInhabs (outside u))

config :: [String] -> (Floor, [Inhab])
config s = (arrayFloorID, listInhab)
  where
    flatString = concat $ transpose $ reverse s

    arrayFloorID = listArray boundPos (map f flatString)
      where f c | c == ' ' = Empty
                | c == 'T' = Target
		| c == 'W' = Wall
		| c == 'P' = Empty
		| c == 'B' = Empty

    listInhab = toListInhab (zip (range boundPos) flatString) (0, (0, 0)) []

    toListInhab [] pusher boxes            = pusher : boxes
    toListInhab ((pos, c) : cs) pusher boxes =
      case c of
        'P' -> toListInhab cs (pusherWeight, pos) boxes
	'B' -> toListInhab cs pusher ((boxWeight, pos) : boxes)
	_   -> toListInhab cs pusher boxes
