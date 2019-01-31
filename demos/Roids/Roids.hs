-- Working toward a simple Asteroids game.
-- 
-- Hit escape to start over with these flying demos.

module Roids where

import Fran
import qualified StaticTypes as S
import Win32Key  -- for vkeys

import Media
import Collide

restarting :: StuffGen -> StuffGen
restarting gen env@(u,_) =
  gen env `untilB` restart u `afterE_` env ==> restarting gen

flyShip :: StuffGen
flyShip env@(u, _) = gen env
 where
   gen = colliderGen (S.zeroVector, S.zeroVector, acc, Ship imB, shooter)
   imB          = flipImage shipBook page `over` soundImage thrustSound
   (acc, angle) = shipControl u
   -- The pages start out pointing up, i.e., pi/2, and rotate clockwise
   -- (negative), so we have to adjust
   page         = shipPagesPerRadian * (pi/2 - angle)
   thrustSound  = volume (magnitude acc) engineSound
   shooter      = keyPress fireKey u `snapshot_` angle



-- Controls

fireKey    = vK_SPACE
restartKey = vK_ESCAPE

restart, fire :: User -> Event User
restart = keyUser restartKey
fire    = keyUser fireKey

keyUser :: VKey -> User -> Event User
keyUser key = nextUser_ (keyPress key)

rotateControl, thrustControl, thrustState :: User -> RealB
rotateControl u = 5 * keysSign vK_LEFT vK_RIGHT u
thrustControl u = 3 * thrustState u
thrustState   u = keysSign vK_UP vK_DOWN u

-- 0, 1, -1, based on key presses
keysSign :: VKey -> VKey -> User -> RealB
keysSign posKey negKey u =
 condB (keyState posKey u) ( 1) $
 condB (keyState negKey u) (-1) 0

keyState :: VKey -> User -> BoolB
keyState key u = stepper False (keyPress   key u -=> True
                            .|. keyRelease key u -=> False )



-- Ship acceleration and angle
shipControl :: User -> (Vector2B, RealB)
shipControl u = (vector2Polar accelMag angle, angle)
 where
   angle      = integral angularVel u
   angularVel = rotateControl u
   accelMag   = thrustControl u

