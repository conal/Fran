-- Some basic types
--
-- Last modified Tue Nov 04 09:13:19 1997

module BaseTypes
       ( RealVal
       , Length
       , Radians
       , Fraction
       , Scalar
       , Time
       , DTime
       , minTime
       , pair
       , fromInt32
       ) where

import Int(int32ToInt, Int32)

type RealVal  = Double
type Length   = RealVal
type Radians  = RealVal
type Fraction = RealVal  -- 0 to 1 (inclusive)

type Time = Double
type DTime = Time                       -- Time deltas, i.e., durations

-- Time that stands for -infinity.  Use for {Behavior}startTime  ###
minTime :: Time
minTime = - 1.0e30

-- For scaling polynomials and vectors.  We should really be using binary
-- type relations, rather than having Double hardwired.

type Scalar = Double

-- For lifting and sections
pair x y = (x,y)

-- From Int32 to Int

fromInt32 :: Num a => Int32 -> a
fromInt32 = fromInt . int32ToInt