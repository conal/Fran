-- Levels.  Adapted from Gary Shu Ling's code

module Levels where

import Types

import Array
import List(transpose)

levels :: [Config]
levels = map parseConfig
           [ board0, board1, board2, board3, board4
           , board5, board6
           ]


parseConfig :: [String] -> Config
parseConfig s = parseAux (zip (range boundLoc) flatString)
                         Nothing []
 where
   -- Collect the pusher and the box locations.
   parseAux :: [(Loc,Char)] -> Maybe Loc -> [Loc] -> Config

   -- Finished the config string.  Build fixed level and yield level
   parseAux [] (Just pusher) boxes =
     Config (parseFixed flatString) pusher boxes

   -- Pusher.  Check for uniqueness
   parseAux ((pos, 'P') : cs) Nothing  boxes =
     parseAux cs (Just pos) boxes
   parseAux ((_  , 'P') : _ ) (Just _) _     =
     error "parseConfig: found second pusher"

   -- Box
   parseAux ((pos, 'B') : cs) mbPusher boxes =
     parseAux cs mbPusher (pos : boxes)

   -- Anything else.  Ignore.
   parseAux ((pos, _  ) : cs) pusher boxes = parseAux cs pusher boxes

   parseFixed :: String -> LevelFixed
   parseFixed s = listArray boundLoc (map f flatString)
     where
       f c | c == ' '  = FixedBlank
           | c == 'T'  = FixedTarget
           | c == 'W'  = FixedWall
           | c == 'P'  = FixedBlank     -- pusher always on empty
           | c == 'B'  = FixedBlank     -- box always on empty
           | otherwise = error ("parseConfig: unknown character " ++ show c)

   flatString = concat $ transpose s



-----------------------------------------------------------------
-- instructions on making your own setting:
--
--  (1) make sure it is of dimensions (maxX, maxY)
--  (2) make sure you only use capital letters and space
--  (3) the mapping is 'W': Wall
--                     'T': Target
--                     'P': Pusher
--                     'B': Box
--                     ' ': Empty
-----------------------------------------------------------------

board0 :: [String]
board0 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWW   WWWWWWWWWWW"
         , "WWWWW   WWWWWWWWWWW"
         , "WWWWW   WWWWWWWWWWW"
         , "WWW      WWWWWWWWWW"
         , "WWW W WW WWWWWWWWWW"
         , "W   W WW WWWWW  WWW"
         , "W           B    TW"
         , "WWWWW WWW WPWW  WWW"
         , "WWWWW     WWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         ]

board1 :: [String]
board1 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWW   WWWWWWWWWWW"
         , "WWWWWB  WWWWWWWWWWW"
         , "WWWWW  BWWWWWWWWWWW"
         , "WWW  B B WWWWWWWWWW"
         , "WWW W WW WWWWWWWWWW"
         , "W   W WW WWWWW  TTW"
         , "W B  B          TTW"
         , "WWWWW WWW WPWW  TTW"
         , "WWWWW     WWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         ]

board2 :: [String]
board2 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWTT  W     WWWWW"
         , "WWWWTT  W B  B  WWW"
         , "WWWWTT  WBWWWW  WWW"
         , "WWWWTT    P WW  WWW"
         , "WWWWTT  W W  B WWWW"
         , "WWWWWWWWW WWB B WWW"
         , "WWWWWW B  B B B WWW"
         , "WWWWWW    W     WWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         ]

board3 :: [String]
board3 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWW     PWWW"
         , "WWWWWWWWWW BWB WWWW"
         , "WWWWWWWWWW B  BWWWW"
         , "WWWWWWWWWWWB B WWWW"
         , "WWWWWWWWWW B W WWWW"
         , "WWTTTT  WW B  B  WW"
         , "WWWTTT    B  B   WW"
         , "WWTTTT  WWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         ]

board4 :: [String]
board4 = [
           "WWWWWWWWWWWW  TTTTW"
         , "WWWWWWWWWWWW  TTTTW"
         , "W    W  B B   TTTTW"
         , "W BBBWB  B W  TTTTW"
         , "W  B     B W  TTTTW"
         , "W BB WB B BWWWWWWWW"
         , "W  B W     WWWWWWWW"
         , "WW WWWWWWWWWWWWWWWW"
         , "W    W    WWWWWWWWW"
         , "W     B   WWWWWWWWW"
         , "W  BBWBB  PWWWWWWWW"
         , "W    W    WWWWWWWWW"
         ]

board5 :: [String]
board5 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWW   WWWWWW"
         , "WWWWWWWWWW WBWW  WW"
         , "WWWWWWWWWW     B WW"
         , "WWWWWWWWWW WWW   WW"
         , "WWTTTT  WW B  BWWWW"
         , "WWTTTT    B BB WWWW"
         , "WWTTTT  WWB  B PWWW"
         , "WWWWWWWWWW  B  WWWW"
         , "WWWWWWWWWW B B  WWW"
         , "WWWWWWWWWWWW WW WWW"
         , "WWWWWWWWWWWW    WWW"
         ]

board6 :: [String]
board6 = [
           "WWWWWWWWWWWWWWWWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         , "WWWWTT  WWWWPWWWWWW"
         , "WWWWTT  WWW   WWWWW"
         , "WWWWTT     BB WWWWW"
         , "WWWWTT  W W B WWWWW"
         , "WWWWTTWWW W B WWWWW"
         , "WWWWWWW B WB  WWWWW"
         , "WWWWWWW  BW B WWWWW"
         , "WWWWWWW B  B  WWWWW"
         , "WWWWWWW  WW   WWWWW"
         , "WWWWWWWWWWWWWWWWWWW"
         ]
