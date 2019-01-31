--------------------------------------------------------------------------
-- The fifteen puzzle, "Erik & Simon version"
--
--   Separate model from visualization !
--
-- Last modified Fri Jul 25 14:32:38 1997 by conal
--------------------------------------------------------------------------

-- Rename to Fifteen2.hs

module Fifteen2 where

import Fran
import qualified StaticTypes as S
import Trace
import Random

--------------------------------------------------------------------------
-- The location and board types.
--------------------------------------------------------------------------

type Loc  = (Int, Int)                  -- (column, row)

type Board = [Loc]      -- First elt is location of blank,
                        -- subsequent elts give location of 1..15

initialBoard = [(i,j) | j <- [0 .. rows-1], i <- [0 .. cols-1]]

rows, cols :: Int
rows = 4
cols = 4

puzzleWidth,  puzzleHeight :: RealVal
(puzzleWidth, puzzleHeight) = (2, 2)

pieceWidth, pieceHeight :: RealVal
pieceHeight = puzzleHeight / fromInt cols
pieceWidth  = puzzleWidth  / fromInt rows

--pieceSize :: S.Vector2
--pieceSize     = vector2XY pieceWidth pieceHeight
--pieceHalfSize = pieceSize ^/ 2

-- Map between locations and points.

locToCenterPos :: Loc -> S.Point2

locToCenterPos (col,row) = S.point2XY x y
 where
   row' = rows - 1 - row
   x = (fromInt col  + 0.5) * pieceWidth  - puzzleWidth /2
   y = (fromInt row' + 0.5) * pieceHeight - puzzleHeight/2

posToLoc :: S.Point2 -> Loc

posToLoc pos = (col, row)
 where
   -- This def comes from symbolically inverting the previous one, taking
   -- round as the inverse of fromIntB
   (x,y) = S.point2XYCoords pos
   col  = round ((x + puzzleWidth /2) / pieceWidth  - 0.5)
   row' = round ((y + puzzleHeight/2) / pieceHeight - 0.5)
   row  = rows - 1 - row'



guts :: Loc -> Board -> Board

guts l@(x,y) board@(blank : locs)
  |  not (x >= 0 && x < cols &&
          y >= 0 && y < rows)
  || not (adjacent l blank)
  = --trace ("guts: bad move " ++ show l) $
    board

  | otherwise
  = --traceShow ("guts: good move " ++ show (l, board) ++ "\n") $
    l : map move locs
  where
    move l' | l == l'   = blank
            | otherwise = l'

    adjacent (x1,y1) (x2,y2) = dx + dy == 1
                             where
                                dx = abs (x1-x2)
                                dy = abs (y1-y2)


puzzle :: Event Loc -> Event Board

puzzle pick = scanlE (flip guts) initialBoard pick

demo1, demo2 :: User -> ImageB

demo1 u = render' be (startTime u)
 where
   be   = puzzle locE
   locE = {- traceE "mouseLocE " TraceOccsE $ -} mouseLocE u

demo2 u = {- randIm `over` -} render' be t0
 where
   be    = puzzle locE
   locE  = randLoc blankLoc randE
   randE = occsE (zip [t0, t0+5 ..] (userRandomInts u))
   t0    = startTime u
   blankLoc = stepper (head initialBoard) (be ==> head)
   -- blankLoc = headB (stepper initialBoard be)
   randIm = moveXY 0 (-0.9) $
            withColor green $
            switcher emptyImage (randE ==> showIm)

-- Click on locations
mouseLocE :: User -> Event Loc

mouseLocE u = (lbp u `snapshot_` mouse u) ==> posToLoc

randLoc :: Behavior Loc -> Event Int -> Event Loc

randLoc blankLoc randInt =
  randInt `snapshot` blankLoc ==> \ (i, loc@(col,row)) ->
  case (i `mod` 4) of
    0 -> (col+1, row)
    1 -> (col, row+1)
    2 -> (col-1, row)
    3 -> (col, row-1)

render'Was :: Event Board -> ImageB

render'Was be  = switcher (render initialBoard) (be ==> render)
 where
   render :: Board -> ImageB

   render (_ : locs) = overs (zipWith renderPiece [1 ..] locs)
    where
     renderPiece n loc =
       move (constantB (locToCenterPos loc S..-. S.origin2)) $
       stretch 2                                             $
       showIm n


render' :: Event Board -> Time -> ImageB

render' be t0 = switcher (render ((initialBoard,initialBoard), t0))
                         (withTimeE (withPrevE be initialBoard) ==> render)
 where
   render :: ((Board,Board), Time) -> ImageB

   render (((_ : prevLocs),(_ : newLocs)), tMove) =
     overs (zipWith3 renderPiece [1 ..] prevLocs newLocs)
    where
     renderPiece n prevLoc newLoc =
       move (lerp2 tMove (tMove + 3)
                   (constantB (locToCenterPos prevLoc))
                   (constantB (locToCenterPos newLoc)) .-. origin2) $
       stretch 2                                                    $
       showIm n

     lerp2 t0 t1 p1 p2 =
       p1 .+^ (timeSince t0 / dt) *^ dp `untilB` timeIs t1 -=> p2
      where
        dp = p2 .-. p1
        dt = constantB (t1 - t0)


---- Misc

userRandomInts u = randomInts (floor (353 * t0)) (floor (173 * t0))
 where t0 = startTime u


---- Testing

es1 = guts (1,0) initialBoard           -- should change

es2 = guts (1,2) initialBoard           -- should not change



{-

To do:

+ How to choose color ?

-}