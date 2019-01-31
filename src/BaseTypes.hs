-- Some basic types
--
-- Last modified Tue Aug 05 11:33:20 1997

module BaseTypes where

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

fromScalar :: Fractional a => Scalar -> a

fromScalar = fromDouble

-- For lifting and sections
pair x y = (x,y)
