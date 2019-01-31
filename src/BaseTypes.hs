-- Some basic types
--
-- Last modified Thu Nov 07 13:58:55 1996

module BaseTypes where

type RealVal  = Double
type Length   = RealVal
type Radians  = RealVal
type Fraction = RealVal  -- 0 to 1 (inclusive)

type Time  = Double

-- For interval and behavior lifting
pair x y = (x,y)
