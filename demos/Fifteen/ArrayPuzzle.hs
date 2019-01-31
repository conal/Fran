-- Try out an idea for using immutable arrays for animations with
-- environments, as described in Appendix B of A "Fifteen Puzzle" in Fran.
-- http://www.research.microsoft.com/scripts/pubDB/pubsasp.asp?RecordID=187

module ArrayPuzzle where

import Fran
import Array
import Win32Key  -- For arrow keys
import IOExts (trace)

type Loc  = (Int,Int)
type LocB = Behavior Loc

rows, cols :: Num a => a
rows = 4; cols = 4

legalLoc :: Loc -> Bool
legalLoc (col,row) = 0 <= col && col < cols && 0 <= row && row < rows

type Dir = Loc

dirs :: [Dir]
dirs@[east,west,north,south] = [(1,0),(-1,0),(0,-1),(0,1)]


puzzle :: Event Dir -> [LocB]
puzzle moveDir = map pieceLoc loc0s
 where
   moveFromTo :: Event (Loc,Loc)
   moveFromTo = moveDir `snapshot` blankLoc
                        ==>        (\ (dir,to) -> (to-dir, to))
                        `suchThat` (legalLoc . fst)

   blankLoc :: LocB
   blankLoc = stepper blankLoc0 (moveFromTo ==> fst)

   moveTos :: Array Loc (Event Loc)
   moveTos = arrayE puzzleBounds moveFromTo

   moveToAt :: LocB -> Event Loc
   moveToAt = (moveTos !*)

   pieceLoc :: Loc -> LocB
   pieceLoc loc0 = locB
    where
      locB = stepper loc0 (moveToAt locB)


-- Starting locations.  Could almost use "range puzzleBounds", but range
-- goes column-major.
blankLoc0 :: Loc; loc0s :: [Loc]
blankLoc0 : loc0s = 
  [ (col, row) | row <- [0 .. rows-1] , col <- [0 .. cols-1] ]

puzzleBounds :: (Loc,Loc)
puzzleBounds = ((0,0),(cols-1,rows-1))


------  Swiped from Fifteen.hs (should go in modules).

puzzleIn  :: User -> Event Dir
puzzleIn u =
  keyPressAny u `assocE` [ (vK_LEFT , west ),
                           (vK_RIGHT, east ),
                           (vK_UP   , north),
                           (vK_DOWN , south) ]

puzzleOutI :: [LocB] -> [ImageB] -> ImageB
puzzleOutI locBs pieceIms =
  overs [ moveTo (locToPoint locB) im | (locB,im) <- zip locBs pieceIms ]

-- Crop one image into puzzle-piece images
cropIms :: ImageB -> [ImageB]
cropIms wholeIm =
  -- For each location, crop the whole image
  -- and move to origin
  [ move (origin2 .-. pos) (crop rect wholeIm)
  | loc <- loc0s
  , let pos  = locToPoint (constantB loc)
        rect = rectFromCenterSize
                 pos pieceSize ]

-- LocB to Point2B conversion
locToPoint :: LocB -> Point2B
locToPoint locB = point2XY x y
 where
   (col, row) = pairBSplit locB
   row' = rows - 1 - row
   x    = (fromIntB col  + 0.5) * pieceWidth  - puzzleWidth /2
   y    = (fromIntB row' + 0.5) * pieceHeight - puzzleHeight/2

anim :: User -> ImageB
anim u = puzzleOutI (puzzle (puzzleIn u)) (cropIms halloween)

main = displayU anim

halloween = stretch 0.5 (importFBitmap "halloweenBig.bmp") `over`
            withColor blue solidImage

importFBitmap name = importBitmap ("c:/Users/Conal/Fran/media/" ++ name)

-- Dimensions
puzzleWidth, puzzleHeight, pieceWidth, pieceHeight   :: RealB
puzzleWidth = 2; puzzleHeight = 2
pieceHeight = puzzleHeight / cols
pieceWidth  = puzzleWidth  / rows

pieceSize :: Vector2B
pieceSize = vector2XY pieceWidth pieceHeight


-- Testing
arrE1 u = array (0,1) [(0, lbp u),(1,rbp u)]

tst1 u = turn a (star 3 5)
 where
   a = stepper 0 $ accumE 0 (e -=> (+ 0.1))
   e = arrE1 u !* i
   i = stepper 0 (charPress '1' u -=> 0 .|.
                  charPress '2' u -=> 1)

-- For locations/directions as pairs
instance (Num a, Num b) => Num (a,b) where
  (a,b) + (a',b') = (a+a',b+b')
  negate (a,b)    = (negate a, negate b)
  -- ...

