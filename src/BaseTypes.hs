-- Some basic types
--
-- Last modified Tue Jan 08 11:00:18 2002

module BaseTypes
       ( RealVal
       , Length
       , Radians
       , Fraction
       , Scalar
       , Time
       , DTime
       , minTime
       , pair , cond
       , fromInt32
       , assoc
       , forkIO'
       ) where

import Int  (int32ToInt, Int32)
import List (find)
import Concurrent (forkIO,yield)

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

cond a b c = if a then b else c

-- From Int32 to Int

fromInt32 :: Num a => Int32 -> a
fromInt32 = fromInt . int32ToInt

-- Isn't this somewhere standard??
assoc :: Eq key => [(key,a)] -> key -> Maybe a
assoc pairs key = fmap snd (find ((== key) . fst) pairs)


-- Spawn a thread & yield.  Somehow needed to prevent immediate thread
-- deadlock.  We still get some deadlock, however.
forkIO' :: IO () -> IO ()
forkIO' io = do forkIO io
                yield