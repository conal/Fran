-- Main module for fifteen puzzle

module Main where

import Puzzle
import UI

import Fran

-- Try numIms, (cropIms charlotte), or (cropIms wigglyCharlotte) as
-- argument to puzzleIm.

puzzleIm :: [ImageB] -> User -> ImageB
puzzleIm ims = puzzleOut ims . goPieces . puzzleIn


main1 = displayU (puzzleIm numIms)
main2 = displayU (puzzleIm (cropIms charlotte))
main3 = displayU (puzzleIm (cropIms wigglyCharlotte))

main = main3

-- charlotte =
--   stretch (wiggleRange 0.5 0.7) (
--     importBitmap "../../media/CharlotteBig.bmp"
--     `over` withColor blue solidImage )

-- main = displayU $ puzzleIm (cropIms wigglyCharlotte)

charlotte = stretch 0.65 (importBitmap "../../media/CharlotteBig.bmp")
            `over` withColor blue solidImage

wigglyCharlotte = stretch (wiggleRange 0.8 1.2) charlotte
