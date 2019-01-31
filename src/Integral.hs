-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).
-- 
-- Last modified Fri Oct 25 16:19:30 1996

module Integral where

import VectorSpace      
import Behavior
import Event
import Until

integral :: VectorSpace v => Behavior v -> Time -> Behavior v

-- Piecewise-linear Euler's method.  Step forward from the starting time,
-- taking regular steps, keeping an accumulator of the integral so far.  A
-- tricky point is that we must force the evaluation of the accumulators,
-- because clients of the integral behavior might skip over several
-- successive linear segments without sampling them (especially if a
-- speeding-up time transform is applied).  Without forcing, the
-- accumulator representation would keep growing and finally blow the
-- heap.

integral dy t0 =
 loop zeroVector dy t0
 where
  loop y0 dy t0 =
    -- Follow a linear path from y0 with a derivative equal to dy@t0.
    -- After epsilon, repeat with a new y0 and dy.
    -- Force the accumulator, so partial computations don't build up.
    force y0 `seq`
    lift1 linearPart time `untilB` timeIs (t0+epsilon) *=> \ t1 ->
      loop (linearPart t1) dy' t1
    where
      linearPart t = y0 `addVector` scaleDy0 (t-t0)

      -- Scale initial derivative value dy0.  Check for the zero scale
      -- factor.  This specialization prevents evaluation of dy0 before
      -- it's ready.  Otherwise, recursive integrals would get into an
      -- evaluation cycle at time t0.
      scaleDy0 0 = zeroVector
      scaleDy0 s = s `scaleVector` dy0

      (dy0,dy') = dy `at` t0

  epsilon = 0.07
