-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).
--
-- This version is modified from ../Integral.hs to use a "user" to get the
-- frame period.  Needs to be made invisible somehow.
-- 
-- Last modified Tue Jul 15 21:44:50 1997

module Integral where

import BaseTypes (Time)
import qualified VectorSpace as VS
import VectorSpaceB
import Behavior
import Event
import BehaviorEvent
import Force

import Trace
import User (User)
import Interaction (updateDone)

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
    untilTimeIs b t f = b `untilB` (timeIs t -=> f)

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
