Second Fran 2D collision demo.  A bunch of balls bounce against each
other and the walls of a room.  Unlike the previous version, this one is
dynamic, i.e., objects do come and go during the life of the animation.

Last modified Mon Jun 30 08:20:26 1997 by conal

> module Collisions2 where

> import Fran
> import Collisions1 hiding (demo)


Start with the main structure of demo itself, and then later fill in the
pieces.  In this version, we are passed in a generator that yields a new
ball, specifying color and initial position and velocity.

> demo :: Gen (Color, Point2, Vector2) -> Int -> ImageB

> demo newBallStart numWalls =
>    foldrB over emptyImage
>      (mapB drawBall balls `appendB` mapB drawWall walls)
>  where
>    walls = toListB (makeWalls numWalls)
> 
>    -- Make the balls, based on start position and velocity and the
>    -- collision event.
>    
>    balls = genListB (newBallStart ==> go)
> 
>    go ((color, pos0, vel0), done) = (ball, done)
>     where
>       ball = startBall color pos0 vel0 collideE
>       collideE = foldrB (.|.) neverE collisions
>       collisions = mapB (ball `collideBall`) balls `appendB`
>                    mapB (ball `collideWall`) walls 

Note: that collideE must be an *event*, foldrB must produce GBehavior
thing, and hence events must be GBehavior's.  I think this will work,
and that intuitively it fills a gap, of events that change with time.
