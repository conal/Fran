-- Main module

module Main where

import Array
import Fran

import Types
import UI
import Play
import Levels

game :: [Config] -> User -> ImageB

game [] u = stretch 3 (stringIm "You did it!")

game (config : levels') u =
  sokoOut config pusherLoc boxLocs left `untilB`
    nextUser_ (predicate (left ==* 0)) u ==> game levels'
 where
   (pusherLoc, boxLocs, left) = playLevel config (sokoIn u)



main = displayU (game levels)