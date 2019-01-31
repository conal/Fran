module SokoBoard where

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
board0 = [				-- to be reversed
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
board1 = [				-- to be reversed
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
board2 = [				-- to be reversed
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
board3 = [				-- to be reversed
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
board4 = [				-- to be reversed
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
board5 = [				-- to be reversed
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
board6 = [				-- to be reversed
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