-- Integration
-- 
-- Last modified Sat Sep 07 23:23:09 1996

module Integral where

import VectorSpace      
import Behavior
import Until

integral :: VectorSpace v => Behavior v -> Time -> Behavior v

integral b t0 =
  Behavior (\ ts -> let (ts',drop_0) =
                          if head ts /= t0 then ((t0:ts),tail) else (ts,id)
                    in
                        drop_0 (integrateList ts' (b `ats` ts')) )

-- This implementation does work with recursively (ODEs), but not
-- efficiently, because the `ats` call starts another evaluation.  The
-- integrateList function works well in recursive situations (igl2 below),
-- because there's explicitly only one list involved.


-- "Integrate" a list of values, xs, with corresponding times, ts.

integrateList :: VectorSpace v => [Time] -> [v] -> [v]

integrateList ts xs =
  -- Euler's algorithm: y_0 = 0; y_{i+1} = y_i+(t_{i+1}-t_i)*x_i
  ys where ys  = zeroVector : zipWith addVector ys (zipWith scaleVector dts xs)
           dts = zipWith (-) (tail ts) ts

-- tests

tstIIg b = take 10 (b `ats` [0.1, 0.2 ..])

tstIg b = (b `ats` [0.1, 0.2 ..]) !! 299  -- b 10

ig1, ig2, ig3 :: Behavior Time
ig1 = integral 1 0
ig2 = integral (2 * time) 0

igl1,igl2 :: [Time]
igl1 = integrateList [0.1, 0.2 ..] igl1
igl2 = map (1-) (integrateList [0.1, 0.2 ..] igl2)

tstIgl t = 
  igl !! (floor (t*10))
  where igl = map (1-) (integrateList [0.1, 0.2 ..] igl)



-- Recursive integrals (ODEs) are pretty inefficient.
ig3 = 1 - integral ig3 0
