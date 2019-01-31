-- This module goes with the Fran Users' Manual, but is largely redundant
-- with the tutorial
--
-- I added artificial "u" arguments to circ, ball1, etc, to eliminate
-- CAFs, but haven't updated the manual.


-- In GHC main has to be in module Main
--module UsersMan where

import Fran    -- Basic Fran functionality
import qualified StaticTypes as S

import Win32Key  -- The VKey type

----------------------------------------------------------------

-- Note functions like circ, ball1, movingBall1, etc, take a "user"
-- argument only to avoid a space leak (the CAF problem).

circ :: User -> ImageB
circ u = stretch (sin time) (withColor red circle)

example1 = circ

----------------------------------------------------------------

ball1, ball2, movingBall1, movingBall2 :: User -> ImageB

ball1 u = stretch 0.3 (withColor red circle)
ball2 u = stretch 0.4 (withColor blue circle)

movingBall1 u = move (vector2XY 0 wiggle) (ball1 u)
movingBall2 u = move (vector2XY wiggle 0) (ball2 u)

example2 u = movingBall1 u `over` movingBall2 u

----------------------------------------------------------------

example3 u = move (mouseMotion u) (ball1 u)

----------------------------------------------------------------

example4 u = withColor col circle where
  col = red  `untilB` lbp u -=> blue

----------------------------------------------------------------

example5 u = withColor col circle where
  col = red `untilB` lbp u -=> blue
                 .|. rbp u -=> green

------------------------------------------------------------------

-- Demonstrates the new event programming style that avoids explicitly
-- recursive state machines and nextUser (or afterE):

example6 u = withColor c circle
 where
   c = switcher red (lbp u -=> blue .|. rbp u -=> red)


example7 u = withColor (fstB state) circle
 where
   state = stepAccum (S.red,S.blue) (lbp u -=> \ (a,b) -> (b,a))


----------------------------------------------------------------

example8 u = move (vector2XY l l) ball
 where
   l = stepper 0 (lbpCounter u)
   ball = stretch 0.3 (withColor red circle)
   lbpCounter :: User -> Event RealVal
   lbpCounter u = withElemE_ (lbp u) [0.1, 0.2 ..]

----------------------------------------------------------------

example9 u = withColor red (moveXY 0 pos (stretch 0.1 circle)) 
 where
   pos = p0 + integral vel u
   vel = v0 + integral acc u
   acc = -0.3
   p0  = -1
   v0  = 1


----------------------------------------------------------------

mouseEvs :: User -> Event S.Vector2
mouseEvs u = lbp u `snapshot_` mouseMotion u

example10 u = withColor red $
              move (stepper S.zeroVector (mouseEvs u)) $
              stretch 0.1 circle

----------------------------------------------------------------

example11 u = withColor red (moveXY 0 pos (stretch 0.1 circle)) 
 where
   pos     = p0 + integral vel u
   vel     = v0 + integral acc u + sumE impulse
   impulse = collide `snapshot_` vel ==> (* (-1.9))
   collide = predicate (pos <=* (-1) &&* vel <* 0) u
   acc     = -1
   p0      = -1
   v0      = 2

sumE :: Num a => Event a -> Behavior a
sumE ev = stepper 0 (scanlE (+) 0 ev)

----------------------------------------------------------------

example12 u = move (mouseMotion u)
                   (stretch 0.2 (withColor red circle))
                `over` 
              move (timeTransform (mouseMotion u) (time - 2))
                   (stretch 0.3 (withColor blue circle))


----------------------------------------------------------------

examples = [example1, example2, example3, example4, example5,
            example6, example7, example8, example9, example10,
            example11, example12]

run k | k >= 1 && k <= length examples = displayU (examples !! (k-1))
      | otherwise = error ("The example number must be between 1 and " ++ 
                           show (length examples) ++ "\n")

{-
main = displayU $ \ u ->
  let change = keyIn nextKeys u -=> 1 .|. keyIn prevKeys u -=> -1 in
  mapSwitcher showExample 1 (scanlE (+) 1 change ==> clamp `afterE` u)
 where
   showExample (k, u) = addTitle k ((examples !! (k-1)) u)
   clamp k = (0 `max` k) `min` (len-1)
   len = length examples
   nextKeys = [vK_SPACE,vK_RIGHT, charToVKey 'N']
   prevKeys = [vK_LEFT, charToVKey 'P']
   charToVKey :: Char -> VKey
   charToVKey = fromInt . fromEnum

   keyIn chars u = keyPressAny u `suchThat` (`elem` chars)
-}

main = displayU $ \ u ->
  mapSwitcher showExample (1,u) (accumE 1 (change u) `afterE` u)
 where
   showExample (k, u) = addTitle k ((examples !! (k-1)) u)
   len = length examples

   change :: User -> Event (Int->Int)
   change u = keyPressAny u `assocE` assocs

   assocs :: [(VKey,Int->Int)]
   assocs = map (`pair` next) nextKeys ++ map (`pair` prev) prevKeys
    where
      next i | i == len  = len
             | otherwise = i+1
      prev 1 = 1
      prev i = i-1

   nextKeys = [vK_SPACE,vK_RIGHT, charToVKey 'N']
   prevKeys = [vK_LEFT, charToVKey 'P']
   charToVKey :: Char -> VKey
   charToVKey = fromInt . fromEnum

addTitle :: Int -> ImageB -> ImageB
addTitle k image = moveXY (-0.5) 0.9 txt `over` image
  where
   txt = stretch 1.5 $ withColor white (stringBIm (constantB ("Example " ++ show k)))
