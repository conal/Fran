-- Fran 2D collision demo.  A bunch of balls bounce against each other and
-- the walls of a room.  This version is not very dynamic, i.e., objects
-- do not come and go during the life of the animation.
--
-- In progress


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
                  deriving Show

startBouncing :: S.Vector2 -> S.Vector2 -> Event Collision
              -> User -> (Vector2B, Vector2B)
startBouncing motion0 vel0 collideE u = (motion, vel)
 where
   motion = constantB motion0 + atRate vel u
   vel = constantB vel0 + atRate acc u + stepper S.zeroVector totImpulse
   acc = -0.1 *^ yVector2  -- a little gravity

   -- This event sums total impulse so far.
   totImpulse :: Event S.Vector2
   totImpulse = scanlE (+) S.zeroVector
                   ((traceE "collideE" TraceOccsE
                    collideE) ==> impulse)

   impulse :: Collision -> S.Vector2
   impulse (Collision {collVel=obstVel, collNormal=obstNormal}) =
     1.95 S.*^ (obstVel `S.dot` obstNormal) S.*^ obstNormal


--------------------------------------------------------------------------
--                                Balls
--------------------------------------------------------------------------

-- What is a ball description?  For now, just motion and velocity.  (The
-- radius is ballRadius, but we could generalize if desired, to any
-- constant, or even time-varying, radius.)

data Ball = Ball { ballIm     :: ImageB
                 , ballMotion :: Vector2B
                 , ballVel    :: Vector2B
                 }

ballRadius :: Floating a => a
ballRadius = 0.2

ballImage :: String -> ImageB
ballImage name = stretch (constantB (2 * ballRadius / imHeight)) im
 where
   (im, _, imHeight) = importBitmapWithSize ("../Media/" ++ name ++
                                             " head black background.bmp")

renderBall :: Ball -> ImageB
renderBall (Ball {ballIm, ballMotion}) = move ballMotion ballIm

-- Something that can collide with a ball.  Include user for predicate or
-- integration.
type BallCollider = User -> Ball -> Event Collision

type BallParam = (ImageB, S.Vector2, S.Vector2)

-- Start a ball going, based on initial conditions and collider, which may
-- be the merging of several colliders.
startBall :: BallCollider -> User -> BallParam -> Ball
startBall collider u (im, motion0, vel0) = ball
 where
   ball      = Ball { ballIm=im, ballMotion=motion, ballVel=vel }
   (motion,vel) = startBouncing motion0 vel0 (collider u ball) u

startBalls :: BallCollider -> User -> [BallParam] -> [Ball]
startBalls collider u = map (startBall collider u)


neverCollider :: BallCollider
neverCollider _ _ = neverE

-- Ball colliding with a floor.  Generalize later.
ballFloorCollider :: BallCollider
ballFloorCollider u (Ball {ballMotion, ballVel}) =
  (predicate hit u `snapshot_` (floorVel - ballVel)) ==> \ collVel ->
  Collision {collVel, collNormal = S.yVector2}
 where
   hit = y <* floorHeight + ballRadius &&* dy <* 0
   y   = snd (vector2XYCoords  ballMotion)
   dy  = snd (vector2XYCoords ballVel)
   floorVel = zeroVector
   -- Switch to the following when I have derivative.
   -- floorVel = vector2XY 0 (derivative floorHeight u)
   floorHeight = - snd (vector2XYCoords (viewSize u))

-- Ball colliding with another ball.
ballBallCollider :: Ball -> BallCollider
ballBallCollider (Ball {ballMotion=obstMotion, ballVel=obstVel})
                 u (Ball {ballMotion, ballVel}) =
  (predicate hit u `snapshot_` relVel
                   `snapshot` (normalize relMotion))
                   ==>  \ (collVel,collNormal) ->
                            Collision {collVel, collNormal}
 where
   relMotion = obstMotion - ballMotion
   relVel    = obstVel    - ballVel
   hit = magnitude relMotion <* ballRadius &&* relMotion /=* zeroVector

mergeBallCollider :: BallCollider -> BallCollider -> BallCollider
mergeBallCollider coll coll' u ball = coll u ball .|. coll' u ball


--------------------------------------------------------------------------
--                               Testing
--------------------------------------------------------------------------

-- Some of my favorite pictures
pat, charlotte, becky, jake :: ImageB
pat       = ballImage "pat"
charlotte = ballImage "charlotte"
becky     = ballImage "becky"
jake      = ballImage "jake"

-- Single ball, bouncing with given collider
demo1 :: BallCollider -> User -> ImageB
demo1 collider u = -- showBIm v `over`
                   renderBall ball
 where
  m0 = S.vector2XY (-1) (-1)
  v0 = S.vector2XY 0.1  0.3
  ball = startBall collider u (pat, m0, v0)
  --Ball {ballVel=v, ballMotion=m} = ball

-- Several balls, bouncing with given collider and each other
demo :: [BallParam] -> BallCollider -> User -> ImageB
demo params initialCollider u = overs (map renderBall balls)
 where
  balls    = map (startBall collider u) params
  collider = foldr mergeBallCollider initialCollider
                   (map ballBallCollider balls {-[]-})

-- ISSUE: Must prevent balls from bouncing off of themselves.  One means
-- is to make colliders require that the colliding objects are moving
-- toward each other, or are at the same position.  Another is to remove
-- self-collisions in demo, i.e., make a separate collider for each ball
-- that carefully excludes that ball from the list.

ballParams1 = map fixP [(pat,-1,-1,0.3,0.3),(jake,1,-1,-0.3,0.3),
                        (charlotte,-0.3,1,0,0), (becky,0.3,1,0,0)]
 where
  fixP (who,x0,y0,dx0,dy0) =
   (who, S.vector2XY x0 y0, S.vector2XY dx0 dy0)

--------------------------------------------------------------------------
--                                 Misc
--------------------------------------------------------------------------

-- derivative :: S.VectorSpace v => Behavior v -> User -> Behavior v
