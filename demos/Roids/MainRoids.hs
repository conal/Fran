-- Testing and "main" module for Roids

import Fran
import qualified StaticTypes as S

import Roids
import Media
import Collide

-- Fly a ship around with the arrow keys.  Look out for the star missile.
-- Shooter disabled (sorry) because of an infinite recursion problem I'm
-- working on.  Hit escape to return to start position.
main = testGen gen3

-- Test out flipbook display
tryBook :: HFlipBook -> User -> ImageB
tryBook book u = flipImage book (30 * (time - constantB t0))
 where t0 = userStartTime u

-- And track the mouse
fly0 :: HFlipBook -> User -> ImageB
fly0 book u = move (mouseMotion u) (tryBook book u)


-- Run the testN examples with displayU

test1 = tryBook pyramidBook

test2 u = moveXY (-1.5 + dt) 0 $ missileImage radius
         `untilB` predicate (radius <=* 0) u -=> emptyImage
 where
   radius    = max - (max-min) * dt / dur
   dt        = time - constantB t0
   t0        = userStartTime u
   max = 0.4; min = 0; dur = 2.5

-- From flyShip.  Turns and makes thrust sound, but doesn't move
test3 u = render (Ship imB)
 where
   imB          = flipImage shipBook page `over` soundImage thrustSound
   (acc, angle) = shipControl u
   -- The pages start out pointing up, i.e., pi/2, and rotate clockwise
   -- (negative), so we have to adjust
   page         = shipPagesPerRadian * (pi/2 - angle)
   thrustSound  = volume (magnitude acc) engineSound

testRoid n = display $ render $ Roid n

-- Orbits until key press
test4 u = render $
          ColliderSS mot vel coll `untilB` keyPressAny u -=> emptySS
 where
   mot  = vector2Polar 1 dt
   vel  = error "no vel in test9"
   coll = Missile (wiggleRange 0.01 0.3)
   dt   = userTime u

-- For testing the genN examples.  Use escape key to restart
testGen gen = displayU $ \ u -> render (restarting gen (u, emptySS))

-- Wedges with "control stack overflow" when you shoot :-(
gen1 = flyShip

gen2 = colliderGen (pos0, vel0, acc, Missile radius, shooter)
  where
    pos0 = S.vector2XY (-1) (-1)
    vel0 = S.vector2XY 0.5  0.5
    acc  = zeroVector
    shooter = neverE
    radius = wiggleRange 0.01 0.3

-- These ones wedge if colliderGen in Collide.hs test for explosion.  But
-- now I know why!  See iMut7 in ../Test.hs for a more explicit test case
-- that tickles the same problem.
gen3 = gen1 `unionGen` gen2
gen4 = gen2 `unionGen` gen1

-- Should be equivalent to "tryGen gen3".  Wedges
test5 u = render (stuff1 `unionSS` stuff2)
 where
   stuff1 = gen1 (u,stuff2)
   stuff2 = gen1 (u,stuff1)


sumE :: Num a => Event a -> Behavior a
sumE ev = stepper 0 (scanlE (+) 0 ev)

          