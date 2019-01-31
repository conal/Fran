-- "Main" module for two-handed navigation demo.

module Main (main) where

import Fran
import Hand
import Navigate

showNav :: HandGen -> HandGen -> ImageB -> (User -> ImageB)
showNav genA genB imb u = renderHand RightH handA   `over`
                          renderHand LeftH  handB   `over`
                          navigate handA handB *% imb
 where
   handA = genA u
   handB = genB u

-- Sample picture
starPic = withColor yellow (star 5 11)

main = displayU (showNav mouseHand stylusOrKeyHand starPic)
