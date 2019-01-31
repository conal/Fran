--------------------------------------------------------------------------
-- The fifteen puzzle
--
-- Last modified Fri Jul 18 15:44:06 1997 by conal
--------------------------------------------------------------------------


module Fifteen4 where

import Fran
import Trace
import RandomIO


type Loc   = (Int, Int)                 -- Board location (column, row)
type LocB  = Behavior Loc
type PMove = (Loc,Loc)                  -- Move (from, to)
type Board = [Event PMove]              -- Move for piece1, piece2, ...


--------------------------------------------------------------------------
-- The puzzle itself, independent from UI (input or visualization).
--------------------------------------------------------------------------

type Puzzle = Event Loc -> (LocB, Board)

puzzle, puzzleTest1, puzzleTest2, puzzleTest3 :: Puzzle

puzzle moveFrom = (blankLoc, pieces)
 where
   pieces = map (pieceMoves legalMove) startPieces

   legalMove = moveFrom `suchThat` validLoc
                        `snapshot` blankLoc
                        `suchThat` adjacent

   blankLoc = stepper startBlank (legalMove ==> fst)

   adjacent ((x1,y1), (x2,y2))  =  abs (x1-x2) + abs (y1-y2) == 1

pieceMoves :: Event PMove -> Loc -> Event PMove
pieceMoves allMoves startLoc = myMoves
 where
   myMoves = (allMoves `snapshot` myLoc `suchThat` fromIsMe) ==> fst
   myLoc = pieceLoc startLoc myMoves
   fromIsMe ((from, to), me)  =  from == me
  
pieceLoc :: Loc -> Event PMove -> LocB
pieceLoc startLoc moves  =  stepper startLoc (moves ==> snd)

validLoc :: Loc -> Bool
validLoc (x,y)  =  x >= 0 && x < cols && y >= 0 && y < rows

-- Nothing ever moves
puzzleTest1 picked = (constantB startBlank,
                      map (const neverE) startPieces)

-- Move everything to picked location
puzzleTest2 picked = (constantB startBlank,
                      map movePiece startPieces)
 where
   movePiece startLoc = picked ==> (startLoc `pair`)

-- Move picked location to (-1,0)
puzzleTest3 picked = (constantB startBlank,
                      map movePiece startPieces)
 where
   movePiece startLoc =
     picked `suchThat` (== startLoc) ==> (`pair` (-1,0))


{-
-- Determine the blank location from a board

blankLocFromBoard :: Board -> LocB

blankLocFromBoard board =
  stepper startBlank (anyE (map ( ==> fst) board))
  --stepper startBlank (head board ==> fst)
  --stepper startBlank (anyE (map ( ==> fst) []))
-}

--------------------------------------------------------------------------
-- The board and conversion between locations and points
--------------------------------------------------------------------------

startLocs@(startBlank : startPieces) =
  [(i,j) | j <- [0 .. rows-1], i <- [0 .. cols-1]]

rows, cols :: Int
rows = 4
cols = 4

puzzleWidth,  puzzleHeight :: RealB
(puzzleWidth, puzzleHeight) = (2, 2)

pieceWidth, pieceHeight :: RealB
pieceHeight = puzzleHeight / fromInt cols
pieceWidth  = puzzleWidth  / fromInt rows

pieceSize :: Vector2B
pieceSize     = vector2XY pieceWidth pieceHeight
pieceHalfSize = pieceSize ^/ 2


locToCenterPos :: LocB -> Point2B

locToCenterPos loc = point2XY x y
 where
   (col, row) = pairBSplit loc
   row' = constantB (rows - 1) - row
   x = (fromIntB col  + 0.5) * pieceWidth  - puzzleWidth /2
   y = (fromIntB row' + 0.5) * pieceHeight - puzzleHeight/2

posToLoc :: Point2B -> LocB

posToLoc pos = pairB col row
 where
   -- This def comes from symbolically inverting the previous one, taking
   -- round as the inverse of fromIntB
   (x,y) = pairBSplit (point2XYCoords pos)
   col  = roundB ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = roundB ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = constantB (rows - 1) - row'


-- The board in which nothing happens

nothingBoard :: Board

nothingBoard = [ neverE | i <- [1..15] ]

--------------------------------------------------------------------------
-- Various interfaces into the puzzle.  Each generates "picked" location
-- event, given the blank location and the user.
--------------------------------------------------------------------------

type Picker = LocB -> User -> Event Loc

nothingPicker, mousePicker, randPicker, randPicker' :: Picker

-- Pick nothing
nothingPicker blankLoc u = neverE

-- Locations clicked on by user.  Ignores blankLoc
mousePicker blankLoc u = lbp u `snapshot_` posToLoc (mouse u)

-- Random locations next to the blank, given some random integers
randPicker blankLoc u = randInt `snapshot` blankLoc ==> neighbor
 where
   randInt = occsE (zip [t0, t0+1 ..] (randoms (floor (t0 * 10))))
   t0      = startTime u
   neighbor (i, (col,row)) =
     case (abs i `mod` 4) of
       0 -> (col+1, row)
       1 -> (col, row+1)
       2 -> (col-1, row)
       3 -> (col, row-1)

-- Simpler version that doesn't track blanks
randPicker' blankLoc u = randInt ==> abs ==> toLoc
 where
   randInt = occsE (zip [t0, t0+0.2 ..] (randoms (floor (t0 * 10))))
   t0      = startTime u
   toLoc i = (i `mod` cols, (i `div` cols) `mod` rows)

--------------------------------------------------------------------------
--   Presentation
--------------------------------------------------------------------------

--  ???
-- type Presenter = Board -> IO ()


renderBoard :: Board -> ImageB

renderBoard board  = overs (zipWith renderPiece board startPieces)

renderPiece :: Event PMove -> Loc -> ImageB

renderPiece moves start@(col,row) =
  move (locToCenterPos loc .-. origin2)                 $
  stretch 2                                             $
  withColor (condB (loc ==* constantB start) green red)  $
  showIm (row * rows + col)
 where
   loc = pieceLoc start moves


-- To do: learn about JH's pretty printing combinators.

-- printBoard :: Board -> IO ()

-- printBoard moveEs =



--------------------------------------------------------------------------
-- Put it together.  For instance, say "demo randPicker puzzle" or
-- "demo mousePicker puzzleTest3".
--------------------------------------------------------------------------

demo :: Picker -> Puzzle -> IO ()

demo picker puzzle = disp imF
 where
   imF u = renderBoard board
    where
      (blankLoc, board) = puzzle pick
      pick              = picker blankLoc u


--------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------



--------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------

fif1 u = renderBoard nothingBoard

-- One piece that moves after three seconds.
fif2 u = renderPiece (occsE [(startTime u + 3, (from, to))]) from
 where
  from = (0,0)
  to   = (cols-1,rows-1)

{-   To do:

+ Testing:

  - withBlank (used in randPicker/puzzleTest3 and mousePicker/puzzle)

  - randLoc (crashes!)

  - puzzle (crashes!)

  x randLoc'

  x render, by making another Puzzle

  x renderPiece

  x pieceLoc

  x pieceMoves

  x demo

+ Generalize "e `whenE` b" a little and reuse for pieceMoves
  (snapshot/suchThat/fst).

+ Provide "presenter" argument to demo when there is a second one.

+ Add arrow key interface.

+ Consider alternate design in which each piece senses only its four
  neighbors, and reports its moves as (dx,dy).  Maybe do this for a
  different game or puzzle.

-}