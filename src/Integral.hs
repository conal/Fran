-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).
-- 
-- Last modified Thu Oct 24 11:39:35 1996

module Integral where

import VectorSpace      
import Behavior
import Event
import Until

integral :: VectorSpace v => Behavior v -> Time -> Behavior v

-- Here's the idea: sample the integrand behavior over a stream of times
-- independent from time streams with which the integral behavior will be
-- sampled.  Then resample the resulting list of integral values as
-- needed.
--
-- Note: we could probably define the integral as a recursive reactive
-- behavior, without having to know the representation of behaviors here.
--
-- The forceList here is necessary in order to avoid a space leak, because
-- the recursion in stepper is not head-strict, i.e., doesn't evaluate the
-- list elements as they are skipped over.  Because these list elements,
-- as produced by integrateList, are cumulative, the delayed computations
-- get increasingly larger, eventually blowing the heap.  Particularly
-- likely when integral is combined with a time transform that maps small
-- global time deltas to large local time deltas.


integral b t0 =
  Behavior (stepper tys ys)
  where
    tys = [t0, t0 + integralEpsilon ..]
    dys = b `ats` tys
    ys  = forceList (integrateList tys dys)
    integralEpsilon = 0.05

-- Simple piecewise-linear resampler.  Crucial note: value of resampling
-- at time t depends on resampled behavior's value strictly *before* t.
-- This property is what allows (possibly mutual) recursions to be broken.
-- Note also that when t==ty0, dys doesn't get reduced.  This special case
-- kicks in only the first time around for the "b `ats` tys" in integral
-- above, and without it, there would be an reduction cycle.
{-

This function has been taken out for the moment since it will blow the stack
when the older stepper function below will not.  Even if you just pass dys into
the older stepper function (and take the tail in the otherwise case) it will
still blow the heap???

stepper tys@(ty0:tys'@(ty1:_)) dys@(~(dy0:dys')) ys@(y0:ys') ts@(t:ts')
  | t <= ty0  =  addSome y0 (t-ty0) dy0 : stepper tys dys ys ts'
  | otherwise =  stepper tys' dys' ys' ts
  where
   addSome y0  0 dy = y0
   addSome y0 dt dy = y0 `addVector` dt `scaleVector` dy0

stepper _ _ _ [] = []
-}

stepper tys@(_:tys'@(ty1:_)) ys@(y0:ys') ts@(t:ts')
  | t <= ty1  =  (y0 : stepper tys ys ts')
  | otherwise =  stepper tys' ys' ts

stepper _ _ [] = []


-- "Integrate" a list of values, dys, with corresponding times, ts.

integrateList :: VectorSpace v => [Time] -> [v] -> [v]

integrateList ts dys = ys
  -- Euler's algorithm: y_0 = 0; y_{i+1} = y_i+(t_{i+1}-t_i)*dy_i
  where 
    ys  = zeroVector : zipWith addVector ys (zipWith scaleVector dts dys)
    dts = zipWith (-) (tail ts) ts

forceList [] = []
forceList (y : ys) = force y `seq` (y : forceList ys) 


-- tests

tstIg b = b `ats` [0.1, 0.2 .. 3]

ig1, ig2, ig3 :: Behavior Time
ig1 = integral 1 0
ig2 = integral (2 * time) 0

igl1,igl2 :: [Time]
igl1 = integrateList [0.1, 0.2 ..] igl1
igl2 = map (1-) (integrateList [0.1, 0.2 ..] igl2)

-- Recursive integrals (ODEs) work just fine
ig3 = (1 - integral ig3 0) :: Behavior Double

