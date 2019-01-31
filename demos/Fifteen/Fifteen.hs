--           A "Fifteen Puzzle" in Fran

--                  Conal Elliott

--     Last modified Tue Jun 16 17:03:56 1998

-- See Fifteen.doc for a full description.

module Main where
import Fran
import Win32Key  -- For arrow keys
import Random

-- UI vs model

puzzleIn  :: User -> Event Dir

data Dir = East | West | North | South
  deriving Enum

puzzleOut :: [LocB] -> ImageB

-- Locations are (column,row) pairs
type Loc  = (Int, Int)
type LocB = Behavior Loc     -- time-varying

puzzle :: Event Dir -> [LocB]

anim :: User -> ImageB
anim u = puzzleOut (puzzle (puzzleIn u))


-- Model decomposition

onePiece :: Event (Dir, Loc) -> Loc
         -> (Event Loc, LocB)

trackBlank :: [Event Loc] -> LocB

puzzle moveDir = locBs
 where
   (moveFroms, locBs) =
     -- Start each piece going with shared
     -- moveWithBlank event and own start
     -- location.  Unzip list of (moveFrom,
     -- locB) pairs into pair of lists.
     unzip [ onePiece moveWithBlank start
           | start <- startLocs ]
   moveWithBlank =
     moveDir `snapshot` (trackBlank moveFroms)

-- Starting locations
blankStart : startLocs = 
  [ (col,row) | row <- [0 .. rows-1]
              , col <- [0 .. cols-1] ]

rows, cols :: Num a => a
rows = 4; cols = 4


-- One puzzle piece

onePiece moveWBlank startLoc = (moveFrom, locB)
 where
   locB = stepper startLoc moveTo
   (moveFrom, moveTo) =
      unzipE (moveWBlank `snapshot` locB
                         `filterE`  moveOkay)

   moveOkay ((dir, bLoc), loc)
    | loc +^ dir == bLoc  = Just (loc, bLoc)
    | otherwise           = Nothing

(+^) :: Loc -> Dir -> Loc
(col,row) +^ dir = case dir of
  West  -> (col-1, row  )
  East  -> (col+1, row  )
  North -> (col  , row-1)
  South -> (col  , row+1)

--  Stylistic alternative

-- (col,row) +^ West  = (col-1, row  )
-- (col,row) +^ East  = (col+1, row  )
-- (col,row) +^ North = (col  , row-1)
-- (col,row) +^ South = (col  , row+1)


-- Tracking the blank spot

trackBlank moveFroms =
  stepper blankStart (anyE moveFroms)


-- User Interface

puzzleIn u = keyPressAny u `filterE` keyDir

keyDir :: VKey -> Maybe Dir
keyDir vkey = assoc [ (vK_LEFT , West ),
                      (vK_RIGHT, East ),
                      (vK_UP   , North),
                      (vK_DOWN , South) ] vkey

-- Another style:

-- puzzleIn u = 
--   keyPressAny u `filterE` 
--     assoc [ (vK_LEFT , West ),
--             (vK_RIGHT, East ),
--             (vK_UP   , North),
--             (vK_DOWN , South) ]

puzzleOut locBs =
  overs [ moveTo (locToPoint locB) im
        | (locB, im) <- zip locBs numIms ]

numIms :: [ImageB]

numIms = [ stretch 2.2 (showIm i)
         | i <- [1 .. rows * cols] ]

-- LocB to Point2B conversion
locToPoint :: LocB -> Point2B
locToPoint locB = point2XY x y
 where
   (col, row) = pairBSplit locB
   row' = rows - 1 - row
   x = (fromIntB col  + 0.5) * pieceWidth
       - puzzleWidth /2
   y = (fromIntB row' + 0.5) * pieceHeight
       - puzzleHeight/2

-- Dimensions
puzzleWidth, puzzleHeight,
 pieceWidth, pieceHeight   :: RealB
puzzleWidth  = 2; puzzleHeight = 2
pieceHeight  = puzzleHeight / cols
pieceWidth   = puzzleWidth  / rows

pieceSize :: Vector2B
pieceSize = vector2XY pieceWidth pieceHeight




-- Some variations

-- Random directions until any key is pressed.
-- Then as in puzzleIn.  Another nice feature
-- might be to remember all moves and then undo
-- them all on request.
puzzleInRand, puzzleInSwitch
 :: User -> Event Dir

-- Random moves
puzzleInRand u =
  updateDone u `withElemE_` random (0,3) 5
               ==>          toInt
               ==>          toEnum

-- Switches between random and normal
puzzleInSwitch u = 
  switcher (puzzleIn u) (
      charPressU 'r' u ==> puzzleInRand
  .|. charPressU ' ' u ==> puzzleIn    )


-- Not in paper:
puzzleInReg, puzzleInSwitch' :: User -> Event Dir

-- Regular one.  Good for cycling.
puzzleInReg u =
  updateDone u `snapshot_` time ==> toDir
 where
   toDir t = toEnum (round (t * 17) `mod` 4)

-- A trickier variation.  It feels a little more
-- modular
puzzleInSwitch' u = 
  switcher (puzzleIn u) (
     nextUser changeMode u ==> uncurry ($))
 where
   changeMode
     :: User -> Event (User -> Event Dir)
   changeMode u =
     charPressAny u `filterE`
       assoc [ ('r', puzzleInRand)
             , (' ', puzzleIn    ) ]

-- cropIms :: ImageB -> [ImageB]
-- cropIms im = map locCrop startLocs
--  where
--    locCrop loc =
--      move (origin2 .-. pos)
--           (crop rect im)
--     where
--       pos  = locToPoint (constantB loc)
--       rect = rectFromCenterSize
--                pos pieceSize

-- Crop one image into puzzle-piece images
cropIms :: ImageB -> [ImageB]
cropIms wholeIm =
  -- For each location, crop the whole image
  -- and move to origin
  [ move (origin2 .-. pos) (crop rect wholeIm)
  | loc <- startLocs
  , let pos  = locToPoint (constantB loc)
        rect = rectFromCenterSize
                 pos pieceSize ]

puzzleOutI :: [LocB] -> [ImageB] -> ImageB
puzzleOutI locBs pieceIms =
  overs [ moveTo (locToPoint locB) im
        | (locB,im) <- zip locBs pieceIms ]

animI :: [ImageB] -> User -> ImageB
animI pieceIms u =
  puzzleOutI (puzzle (puzzleInSwitch u))
             pieceIms

anim' = animI numIms

-- An experiment: I grabbed the number-based puzzle screen image.  See how
-- much faster it is to import and chop.  Hmm... it does make a noticeable
-- difference.
numIms' = cropIms (stretch 0.5 $ importBitmap "numbersBig.bmp")

halloween = 
  -- stretch 0.65 (importFBitmap "CharlotteBig.bmp")
  -- stretch 0.5 (importBitmap "halloween.bmp")
  stretch 0.5 (importFBitmap "halloweenBig.bmp")
  `over` withColor blue solidImage

anim2 = animI (cropIms halloween)

anim3 = animI (cropIms (
          stretch (slower 2 $ wiggleRange 0.8 1.2)
                  halloween))

anim4 = animI (cropIms (
          stretch 1 (importBitmap "Erik.bmp")
          `over` withColor blue solidImage))

----  Misc

importFBitmap name =
  importBitmap ("c:/Users/Conal/Fran/media/"
                ++ name)

main = displayU anim3
       -- displayUMonC white anim3  -- for presentations


-- Move to Fran/src/Event.hs

unzipE :: Event (a,b) -> (Event a, Event b)
unzipE ev = (ev ==> fst, ev ==> snd)


-- -- Move to Fran/src/User.hs
-- userRandInts :: (Int, Int) -> User -> [Int]

charPressU :: Char -> User -> Event User
charPressU char = nextUser_ (charPress char)

