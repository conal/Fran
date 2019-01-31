-- "Main" module for two-handed navigation demo.
-- 
-- If you don't have a tablet, replace "stylusHand" with "keyHand" in
-- main. Then use shift to grab and arrow keys to move. (To do: make
-- tablet availability a testable property of users, so this change can be
-- done automatically.)

module Main (main) where

import Fran
import Hand
import Navigate

showNav :: (User -> Hand) -> (User -> Hand) -> ImageB -> (User -> ImageB)
showNav genA genB imb u = render handA   `over`
                          render handB   `over`
                          navigate handA handB u *% imb
 where
   handA = genA u
   handB = genB u

-- Sample picture
starPic = withColor yellow (star 5 11)

main = displayU (showNav mouseHand stylusHand starPic)
