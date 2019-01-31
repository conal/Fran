-- This module goes with the Fran Users' Manual, but is largely redundant
-- with the tutorial

-- In GHC main has to be in module Main
--module UsersMan where

import Fran    -- Basic Fran functionality
import qualified StaticTypes as S

import Win32Key  -- The VKey type

----------------------------------------------------------------

circ :: ImageB
circ = stretch (sin time) (withColor red circle)

example1 u = circ

----------------------------------------------------------------

ball1, ball2, movingBall1, movingBall2 :: ImageB

ball1 = stretch 0.3 (withColor red circle)

movingBall1 = move (vector2XY 0 wiggle) ball1

ball2 = stretch 0.4 (withColor blue circle)

movingBall2 = move (vector2XY wiggle 0) ball2

example2 u = movingBall1 `over` movingBall2

----------------------------------------------------------------

example3 u = move (mouseMotion u) ball1

----------------------------------------------------------------

example4 u = withColor (doRed u) circle where
  doRed, doBlue :: User -> ColorB
  doRed  u = red  `untilB` (lbpU u ==> doBlue)
  doBlue u = blue `untilB` (lbpU u ==> doRed)

----------------------------------------------------------------

example5 u = withColor (c red u) circle where
  c :: ColorB -> User -> ColorB
  c cl u = cl `untilB` ((lbpU u ==> c blue)
                        .|. 
                        (rbpU u ==> c green))

----------------------------------------------------------------

switch2 :: Behavior a -> Behavior a -> User -> Behavior a
switch2 v1 v2 u = v1 `untilB` lbpU u ==> switch2 v2 v1

ball61 u = withColor (switch2 red green u) (stretch 0.4 circle) 
ball62 u = withColor (switch2 green red u) (stretch 0.6 circle) 

example6 u = ball61 u `over` ball62 u

----------------------------------------------------------------

lbpCounter :: User -> Event (RealVal)
lbpCounter u = withElemE_ (lbpU u) [0.1, 0.2 ..]

example7 u =
  let l = stepper 0 (lbpCounter u)
      ball = stretch 0.3 (withColor red circle)
   in
      move (vector2XY l l) ball

----------------------------------------------------------------

gravity8 :: RealB

gravity8 = -0.1

velocity8, position8 :: User -> RealB

velocity8 u = integral gravity8 u

position8 u = integral (velocity8 u) u

example8 u = withColor red (moveXY 0 (position8 u) (stretch 0.1 circle))

----------------------------------------------------------------

mouseEvs :: User -> Event S.Vector2
mouseEvs u = lbp u `snapshot_` mouseMotion u

example9 u = withColor red $
             move (stepper S.zeroVector (mouseEvs u)) $
             stretch 0.1 circle

----------------------------------------------------------------

gravity10 :: RealB
gravity10 = -0.6

velocity10 :: RealB -> User -> RealB
velocity10 v0 u = v0 + integral gravity10 u

-- Assume floor is at y = -1
-- Bounces with only 0.9 of the velocity each time

position10' x0 v0 u = p where
   p       = x0 + integral v u
   v       = velocity10 v0 u + sumE impulse
   impulse = collide `snapshot_` v ==> (* (-1.9))
   collide = predicate (p <=* (-1) &&* v <* 0) u
   

sumE :: Num a => Event a -> Behavior a
sumE ev = stepper 0 (scanlE (+) 0 ev)

example10 u = withColor red (moveXY 0 (position10' 0 0 u)
                                      (stretch 0.1 circle))

----------------------------------------------------------------

example11 u = move (mouseMotion u)
                   (stretch 0.2 (withColor red circle))
                `over` 
              move (timeTransform (mouseMotion u) (time - 2))
                   (stretch 0.3 (withColor blue circle))

----------------------------------------------------------------

-- These are not a formal part of Fran

lbpU = nextUser_ lbp  
rbpU = nextUser_ rbp  
predicateU b u = nextUser_ (predicate b) u  

----------------------------------------------------------------

examples = [example1, example2, example3, example4, example5,
            example6, example7, example8, example9, example10, example11]

run k | k >= 1 && k <= length examples = displayU (examples !! (k-1))
      | otherwise = error ("The example number must be between 1 and " ++ 
                           show (length examples) ++ "\n")

main = displayU (showExamples 1)

showExamples k u =
   addTitle k ((examples !! (k-1)) u)
              `untilB` keyIn nextKeys u ==> showExamples (if k == len then k else k+1)
                       .|.
                       keyIn prevKeys u ==> showExamples (if k == 1 then k else k-1)
 where  
  len = length examples
  nextKeys = [vK_SPACE,vK_RIGHT, charToVKey 'N']
  prevKeys = [vK_LEFT, charToVKey 'P']
  charToVKey :: Char -> VKey
  charToVKey = fromInt . fromEnum


addTitle :: Int -> ImageB -> ImageB
addTitle k image = moveXY (-0.5) 0.9 txt `over` image
  where
   txt = stretch 1.5 $ withColor white (stringBIm (constantB ("Example " ++ show k)))

keyIn :: [VKey] -> User -> Event User
keyIn chars u = nextUser_ (\u' -> keyPressAny u' `suchThat` (`elem` chars)) u
