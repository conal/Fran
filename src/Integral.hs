-- Integration.  Handles systems of mutually recursive integral behaviors
-- (ODEs).
-- 
-- Last modified Mon Sep 16 22:18:39 1996

module Integral where

import VectorSpace      
import Behavior
import Event
import Until

integral :: VectorSpace v => Behavior v -> Time -> Behavior v

-- Here's the idea: sample the integrand behavior over a stream of times
-- independent from time streams with with the integral behavior will be
-- sampled.  Then resample the resulting list of integral values as
-- needed.
--
-- Note: we could probably define the integral as a recursive reactive
-- behavior, without having to know the representation of behaviors here.

integral b t0 =
  Behavior (stepper ts ys)
  where
    ts = [t0, t0 + integralEpsilon ..]
    ys = integrateList ts (b `ats` ts)
    integralEpsilon = 0.05

-- Simple piecewise-constant resampler.  Crucial note: value of resampling
-- at time t depends on resampled behavior's value strictly *before* t.
-- This property is what allows (possibly mutual) recursions to be broken.

stepper txs@(_:txs'@(tx1:_)) xs@(x0:xs') ts@(t:ts')
  | t <= tx1  =  x0 : stepper txs xs ts'
  | otherwise =  stepper txs' xs' ts

stepper _ _ [] = []

-- "Integrate" a list of values, xs, with corresponding times, ts.

integrateList :: VectorSpace v => [Time] -> [v] -> [v]

integrateList ts xs =
  -- Euler's algorithm: y_0 = 0; y_{i+1} = y_i+(t_{i+1}-t_i)*x_i
  ys where ys  = zeroVector : zipWith addVector ys (zipWith scaleVector dts xs)
           dts = zipWith (-) (tail ts) ts

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

