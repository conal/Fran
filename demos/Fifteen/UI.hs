-- Fifteen puzzle user interface

module UI where

import Fran

import Grid
import Win32Key

-- Convert user input to directional move requests
puzzleIn :: User -> Event Dir
puzzleIn u = 
  keyPressAny u `filterE` assoc [ (vK_LEFT , West )
                                , (vK_RIGHT, East )
                                , (vK_UP   , North)
                                , (vK_DOWN , South)
                                ]

-- Given piece appearances, convert locations to puzzle appearance
puzzleOut :: [ImageB] -> [LocB] -> ImageB
puzzleOut pieceIms locBs = overs (zipWith moveToLoc locBs pieceIms)
 where
   moveToLoc loc im = move (locToVecB loc) im


-- Pieces as numerals
numIms :: [ImageB]
numIms = [ stretch 2 (showIm n) | n <- [1 ..] ]

-- Pieces as image croppings
cropIms :: ImageB -> [ImageB]
cropIms im = map (locCropped im) loc0s

