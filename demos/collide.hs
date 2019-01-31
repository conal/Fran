-- Fran 2D collision demo.  A bunch of balls bounce against each other and
-- the walls of a room.  This version is not very dynamic, i.e., objects
-- do not come and go during the life of the animation.
-- 
-- Last modified Thu Jul 24 11:34:10 1997 by conal

module Collide where

import Fran
import qualified StaticTypes as S


--------------------------------------------------------------------------
--                              Collisions
--------------------------------------------------------------------------

-- A collision description, for now, is just the velocity of the obstacle
-- relative to the moving object, and normal vector coming out of the
-- obstacle at point of impact.  (Both quantities are snap-shotted at the
-- moment of collision.)  Simple, but sufficient for variously shaped
-- obstacles.  This information is really only adequate with the moving
-- object is a ball.  More generally, a moving object would also need to
-- know where it got hit, in order to impart a spin.  For now, however, we
-- only move balls around, and surfaces are frictionless.  Consequently,
-- impulses always pass through the center of mass, and so there is no
-- spin.

data Collision = Collision { collVel, collNormal :: S.Vector2 }

startBouncing :: S.Point2 -> S.Vector2 -> Event Collision
              -> User -> (Point2B, Vector2B)
startBouncing pos0 vel0 collideE u = (pos, vel)
 where
   pos = constantB pos0 .+^ atRate vel u
   vel = constantB vel0 + atRate acc u + stepper S.zeroVector totImpulse

   acc :: Vector2B
   acc = -0.1 *^ yVector2  -- a little gravity

   -- This event sums total impulse so far.
   totImpulse :: Event S.Vector2
   totImpulse = scanlE (+) S.zeroVector (collideE ==> impulse)

   impulse :: Collision -> S.Vector2
   impulse (Collision {collVel=obstVel, collNormal=obstNormal}) =
     1.95 S.*^ (obstVel `S.dot` obstNormal) S.*^ obstNormal


--------------------------------------------------------------------------
--                                Balls
--------------------------------------------------------------------------

-- What is a ball description?  For now, just position and velocity.  (The
-- radius is ballSize, but we could generalize if desired, to any
-- constant, or even time-varying, radius.)

data Ball = Ball { ballColor :: ColorB
                 , ballPos   :: Point2B
                 , ballVel   :: Vector2B
                 }

-- For now, all the same size
-- ballSize :: Fractional a => a
-- ballSize = 0.1

ballIm :: ImageB
ballSize :: RealVal
(ballIm, _, ballSize) = importHead "charlotte"

importHead name =
  importBitmapWithSize ("../Media/" ++ name ++ " head black background.bmp")


renderBall :: Ball -> ImageB
renderBall (Ball {ballColor=color, ballPos=pos}) =
  move (pos .-. origin2)        $
  stretch (constantB ballSize)  $
  withColor color               $
  ballIm


-- Something that can collide with a ball.  Include user for predicate or
-- integration.
type BallCollider = User -> Ball -> Event Collision

-- Start a ball going, based on initial conditions and collider, which may
-- be the merging of several colliders.
startBall :: ColorB -> S.Point2 -> S.Vector2 -> BallCollider -> User -> Ball
startBall color pos0 vel0 collider u = ball
 where
   ball      = Ball { ballColor=color, ballPos=pos, ballVel=vel }
   (pos,vel) = startBouncing pos0 vel0 (collider u ball) u


mergeBallColliders :: [BallCollider] -> BallCollider
mergeBallColliders colliders u ball =
  anyE (map (\ coll -> coll u ball) colliders)


-- Ball colliding with a floor.  Generalize later.
ballFloorCollider :: BallCollider
ballFloorCollider u (Ball {ballPos, ballVel}) =
  (predicate hit u `snapshot_` (floorVel - ballVel)) ==> \ collVel ->
  Collision {collVel, collNormal = S.yVector2}
 where
   hit = y <* floorHeight + constantB ballSize &&* dy <* 0
   y   = snd (pairBSplit (point2XYCoords  ballPos))
   dy  = snd (pairBSplit (vector2XYCoords ballVel))
   floorVel = vector2XY 0 (derivative floorHeight u)
   floorHeight = - sndB (vector2XYCoords (viewSize u))


--------------------------------------------------------------------------
--                               Testing
--------------------------------------------------------------------------

-- Single ball

demo1 :: BallCollider -> User -> ImageB
demo1 collider u = renderBall (startBall green p0 v0 collider u)
 where
  p0 = S.point2XY  (-1) (-1)
  v0 = S.vector2XY 0.1  0.1


collTst1 = demo1 ballFloorCollider

--------------------------------------------------------------------------
--                                 Misc
--------------------------------------------------------------------------

derivative :: S.VectorSpace v => Behavior v -> User -> Behavior v

-- For now, just sample regularly and take differences.  To do: replace
-- with better algorithm.
derivative b u = stepper S.zeroVector (scanlE derivF S.zeroVector sampler)
 where
  derivF sample prev = (sample S.^-^ prev) S.^/ dt
  sampler = alarmE (startTime u) dt `snapshot_` b
  dt = 0.1
