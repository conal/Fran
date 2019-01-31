First Fran 2D collision demo.  A bunch of balls bounce against each other
and the walls of a room.  This version is not dynamic, i.e., objects do
not come and go during the life of the animation.

Last modified Thu Jul 17 10:47:25 1997 by conal

> module Collisions1 where

> import Fran


We'll start with the main structure of demo itself, and then later fill in
the pieces.

> demo :: Int -> Int -> ImageB

> demo numBalls numWalls =
>    overs (map drawBall balls ++ map drawWall walls)
>  where
>    walls = makeWalls numWalls
> 
>    -- Make the balls, based on start position and velocity and the
>    -- collision event.
>    balls = zipWith4 startBall colors pos0s vel0s collides
>    
>    -- Collisions.  For each ball, test for collision with any ball
>    -- or any wall.
>    collides = [ anyE (collisions b) | b <- balls ]
>    collisions b = map (b `collideBall`) balls ++
>                   map (b `collideWall`) walls
> 
>    colors = [ colorHSL (constantB (360 * i)) 0.5 0.5
>             | i <- [1 .. numBalls]
>             ]
>
>    pos0s = map fitWall (randomDoubles 1 2)
>    vel0s = map (subtract 0.5) (randomDoubles 2 3)


Radius of balls and room.  (The containing room is a regular polygon.)

> ballSize = 1 
> roomSize = 10


What is a ball description?  For now, just position and velocity.  (The
radius ballSize, but we could generalize if desired, to any constant,
or even time-varying, radius.)

> data BallDesc = BallDesc Point2B Vector2B

To draw a ball:

> drawBall :: BallDesc -> ImageB

> drawBall (BallDesc color center _) =
>   move center (withColor color circle)


Walls

A wall description is just a pair of possibly moving points.

> data WallDesc = WallDesc Point2B Point2B

> drawWall :: WallDesc -> ImageB

> drawWall (WallDesc start end) = lineSeg start end


> makeWalls :: Int -> [ WallDesc ]

> makeWalls n = zipWith WallDesc corners (tail corners)
>  where
>    -- All the corners, with first = last
>    corners = [ point2Polar (2 * pi * i / n) roomSize | i <- [0 .. n] ]


A collision description, for now, is just the velocity of the obstacle
relative to the ball, and normal vector coming out of the obstacle at
point of impact.  (Both quantities are snap-shotted at the moment of
collision.)  Simple, but sufficient for variously shaped obstacles.  (More
generally, a moving object would also need to know where it got hit, in
order to impart a spin.  For now, however, we only move balls around, and
surfaces are frictionless.  Consequently, impulses always pass through the
center of mass, and so there is no spin.)

> data CollDesc = (Vector2, Vector2)


Now we can start a ball, given initial position and velocity, and a
collision event.

> startBall :: Point2 -> Vector2 -> Event CollDesc -> BallDesc

> startBall pos0 vel0 collideE = pos
>  where
>    pos = lift0 p0 .+^ atRate vel
>    vel = atRate acc + totImpulse
> 
>    totImpulse =
>      accumB (+) (constantB v0) (collideE ==> impulse ==> constantB)
>
>    impulse (relObstVel, obstNormal) =
>      1.95 *^ (relObstVel `dot` obstNormal) *^ obstNormal

[Efficiency problem: in totImpulse, the impulses are lifted (constantB)
and then summed, but would be much faster (without extensive optimization)
if summed and then lifted.  I think doing this requires accumulation in
the event, and then maybe a function that turns events into
step-function-sytle behaviors, or more generally takes an (Event a) and an
(a -> Behavior b), possibly constantB, into a (Behavior b).  All of these
functions are easy to provide.]


Now to describe a collision between two balls.

> collideBall :: BallDesc -> BallDesc -> Event CollDesc

> collideBall (BallDesc _ center vel) (BallDesc _ centerObst velObst) =
>    hit `snapshot` pairB relObstVel (normalize obstToBall) ==> snd
>  where
>    hit = predicate (magnitude obstToBall == 2 * ballSize)
>    obstToBall = center .-. centerObst
>    relObstVel = velObst - vel


Then a ball colliding with a wall.

> collideWall :: BallDesc -> WallDesc -> Event CollDesc

[Work out and fill in ...]


Unless I've forgotten something, these definitions complete the
example.  Here are some next steps:

* Make the set of balls dynamic.  Have them disappear after a while, for
  some reason, e.g., hitting a special wall, time-out, being picked by
  the user, or exhausted "hit points".  Also, have a way to add new
  balls.  Try to make this work via my the ListB type constructor, and a
  set of ListB combinators.

* Restructure for extensibility, so that one can add new types of
  objects.

