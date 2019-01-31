-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).  Differentiation is also here.
--
-- To do:
--
-- + Switch from trivial Euler's implementation to RK4 soon.  (See old
--   commented-out RK4 code below.)
--
-- + If I do keep the time stream interface, then replace the User
--   argument with a t0 argument.  Better yet would be switching to local
--   zero start times, and eliminating the t0 argument.  The problem
--   with removing the user argument is that I need it for determining
--   step size

module Integral where

import BaseTypes -- (Time)
import qualified VectorSpace as VS
import VectorSpaceB
import Behavior
import Event
--import BehaviorEvent
--import Force
import GBehavior
import BehaviorEvent

import IOExts ( trace )
import User

integral_sample, derivative_sample :: User -> Event ()
integral_sample u = alarmE (userStartTime u) 0.05
                    -- updateDone u -=> ()
derivative_sample = integral_sample

integral, atRate :: VS.VectorSpace v => Behavior v -> User -> Behavior v

integral b u = bInt
 where
   bInt     = switcher zeroVector newPiece
   newPiece = sample `snapshot` pairB b bInt ==> extrapolate
   sample   = withTimeE_ (userTimeIs 0 u .|. makeStep)
   makeStep = updateDone u -=> ()
              -- alarmE (userStartTime u) 0.1  -- For ..\demo\ReplayTut.hs
   extrapolate (t0,(dy0, y0)) =
     constantB y0 ^+^ (time - constantB t0) *^ constantB dy0
     -- The following may be faster
     -- lift1 (\t -> y0 VS.^+^ (t - t0) VS.*^ dy0) time

-- Derivative.  For now implemented by sampling & difference.
-- Warning: give initial bogus value, because it takes initial value to be
-- the zero vector.
-- 
-- With multi-parameter type classes, try to make this function work over
-- affine spaces and produce a behavior from the corresponding vector space.

derivative, rate :: VS.VectorSpace v => Behavior v -> User -> Behavior v
derivative b u =
  stepper VS.zeroVector $
    withTimeE (sample `snapshot_` b) `withPrevE` (VS.zeroVector, t0) ==> deriv
 where
   deriv ((xPrev, tPrev), (xNow, tNow)) | tNow > tPrev  = 
     (xNow VS.^-^ xPrev) VS.^/ (tNow - tPrev)
   sample = updateDone u
   t0 = userStartTime u


-- Synonyms
atRate = integral
rate   = derivative

{-
-- An older version.

integral b u = integralFrom b (userStartTime u)

integralFrom b t0 =
  samplerB (\ ts -> let (ts',drop_0) =
                          if head ts /= t0 then ((t0:ts),tail) else (ts,id)
                    in
                      drop_0 (integrateList ts' (b `ats` ts')) )

-- "Integrate" a list of values, xs, with corresponding times, ts.

integrateList :: VS.VectorSpace v => [Time] -> [v] -> [v]
integrateList ts xs =
  -- Euler's algorithm: y_0 = 0; y_{i+1} = y_i+(t_{i+1}-t_i)*x_i
  ys where ys  = VS.zeroVector : zipWith (VS.^+^) ys (zipWith (VS.*^) dts xs)
           dts = zipWith (-) (tail ts) ts

-}

{-

-- Approximate Runge-Kutta method.  Step forward from the starting time,
-- taking regular steps, keeping an accumulator of the integral so far.  A
-- tricky point is that we must force the evaluation of the accumulators,
-- because clients of the integral behavior might skip over several
-- successive linear segments without sampling them (especially if a
-- speeding-up time transform is applied).  Without forcing, the
-- accumulator representation would keep growing and finally blow the
-- heap.

integral :: (Forceable v, VS.VectorSpace v) => 
	    Behavior v -> User -> Behavior v
-- Start at time t0 with the initial value of zero.  This is crucial for
-- recursive integrals to ensure we don't yet sample b at t0.  After time
-- t0, we can legitimately sample b at t0.  Then if b is defined in terms
-- of y (the integral) we know the value of the integral at t0
-- (zeroVector).
integral b u =
   zeroVector `untilB` timeIs t0 -=>
   rungeKuttaStep b t0 VS.zeroVector (stepSize u)
 where
  t0 = startTime u

-- The initial step size requires a reasonable guess to ensure the system
-- starts up OK.  This needs further work.

initial_h :: Double
initial_h = 0.5

-- Uses the updatePeriod behavior to work out a suitable step size for the
-- integration.  It also uses the previos step size, so the step size
-- changes gradually.  This is a kind of damping which helps to reduce
-- unnecessary fluctuations in the step size if the update period is
-- fluctuating.  The since updatePeriod behavior affects the step size it
-- is important that a reasonable initial updatePeriod value is chosen,
-- although damping ensures that we only require the initial updatePeriod
-- to be very roughly set since the first few updates will not be affected
-- too much.

stepSize :: User -> RealB

stepSize u = -- 0.1
             (stepper initial_dfp interp_dfp) / stepsPerUpdate
  where
    interp_dfp = scanlE (lerp stepSizeDamping)
                        initial_dfp (updateDone u)

    lerp x a b = x*a + (1-x)*b          -- linear interpolation

    stepSizeDamping = 0.5               -- weight of new period
    stepsPerUpdate  = 1
    initial_dfp     = 0.1               -- arbitrary guess


rungeKuttaStep ::  (Forceable v, VS.VectorSpace v) => 
	           Behavior v -> Time -> v -> Behavior Double 
	        -> Behavior v
rungeKuttaStep b tn yn stepBehavior
  = -- trace ("rungeKuttaStep: " ++ show tn ++ "\n") $
    -- Force the accumulator, so partial computations don't build up.
    force yn `seq`

    -- Follow a various linear paths which start at yn,  with a derivatives equal to
    -- k1/h, k2/h, k3/h and k4/h in the Runge-Kutta method.
    --trace ("Runge-Kutta: tn == " ++ show tn ++ "\n") $

    linearFrom_yn1 `untilTimeIs` tnMid $

    linearFrom_yn2 `untilTimeIs` tnJustPastMid $

    linearFrom_yn3 `untilTimeIs` tnJustBeforeEnd $

    linearFrom_yn4 `untilB` again

  where
    (h, stepBehavior') = stepBehavior `at` tn

    -- At tnEnd we have done a whole Runge-Kutta step.  Snapshot y and do
    -- another.
    again = timeIs tnEnd `snapshot_` linearFrom_yn4 ==> \ ynEnd ->
            rungeKuttaStep bEnd tnEnd ynEnd stepBehavior'

    -- To improve the layout above (by facilitating using $).
    untilTimeIs b t f = b `untilB` timeIs t -=> f

    half_h  = h/2 

    epsilon = h*1.0e-5  -- h+epsilon is the point just past the mid where we
			-- obtain k3 of the runge kutta.

    tnMid           =    tn + half_h
    tnJustPastMid   = tnMid + epsilon
    tnJustBeforeEnd = tnEnd - epsilon
    tnEnd           =    tn + h

    (k1_over_h, bMid)           =              b `at` tn
    (k2_over_h, bJustPastMid)   =           bMid `at` tnMid
    (k3_over_h, bJustBeforeEnd) =   bJustPastMid `at` tnJustPastMid
    (k4_over_h, bEnd)           = bJustBeforeEnd `at` tnJustBeforeEnd 

    linearFrom_yn1 = constantB yn ^+^ timeSince tn *^ constantB k1_over_h 
    linearFrom_yn2 = constantB yn ^+^ timeSince tn *^ constantB k2_over_h
    linearFrom_yn3 = constantB yn ^+^ timeSince tn *^ constantB k3_over_h
    linearFrom_yn4 = constantB yn ^+^ timeSince tn *^
	    (constantB ( ((1/6) VS.*^ k1_over_h) VS.^+^
                        (((1/3) VS.*^ k2_over_h) VS.^+^
                        (((1/3) VS.*^ k3_over_h) VS.^+^
                         ((1/6) VS.*^ k4_over_h))) ))
-}
