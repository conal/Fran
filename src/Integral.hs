-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).
-- 
-- Last modified Sun Nov 10 16:18:24 1996

module Integral where

import qualified VectorSpace as VS
import Fuzzy
import Behavior
import VectorSpaceB
import Event
import Until
import Force

integral :: (Forceable v, VS.VectorSpace v) =>
              Behavior v -> Time -> Behavior v

-- Piecewise-linear Euler's method.  Step forward from the starting time,
-- taking regular steps, keeping an accumulator of the integral so far.  A
-- tricky point is that we must force the evaluation of the accumulators,
-- because clients of the integral behavior might skip over several
-- successive linear segments without sampling them (especially if a
-- speeding-up time transform is applied).  Without forcing, the
-- accumulator representation would keep growing and finally blow the
-- heap.
--
-- Also, explicitly start out as the zero vector just for an instant, in
-- order to avoid sampling the integrand at t0.  This is necessary in the
-- case of recursive integrals (ODEs), to avoid an infinite evaluation
-- cycle.

integral dy t0 =
 zeroVector `untilB` timeIs t0 -=>
 loop dy t0 VS.zeroVector
 where
  loop dy t0 y0 =
    -- Follow a linear path from y0 with a derivative equal to dy@t0.
    -- After epsilon, repeat with a new y0 and dy.
    -- Force the accumulator, so partial computations don't build up.
    force y0 `seq`
    linearPart `untilB` nextStep +=> loop dy'
    where
      linearPart = lift0 y0 ^+^ (time - lift0 t0) *^ lift0 dy0 

      -- Take another step, with new accumulated value
      nextStep = (timeIs (t0+epsilon) `snapshot` linearPart) ==> snd

      (dy0,dy') = dy `at` t0

  epsilon = 0.07
