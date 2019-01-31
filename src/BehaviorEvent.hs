-- Event combinators that involve events.  Separated out from Event.hs to
-- avoid a mutual module recursion.
--
-- Last modified Tue Aug 05 16:05:53 1997
--
-- To do:
--
--  + Resolve Forceable in snapshot and snapshot_

module BehaviorEvent where

import BaseTypes (pair)
import Event
import User
import Behavior
import Interaction

import Trace
import MVar                             -- for forkIO, which is for testing

infixl 9 `snapshot`, `snapshot_`, `whenE`

-- The event "e `snapshot` b" occurs at the time te when e occurs.  Its
-- value is e's value together with a snapshot of b at te.  A counterpart
-- to afterE.

snapshot :: {- Forceable b => -} Event a -> Behavior b -> Event (a, b)

Event possOccs `snapshot` b = Event (loop possOccs b)
 where
   loop [] _ = []

   -- Pair up the event data, if any, with the behavior sample.  Do I
   -- need to force y ?  Sometimes yes.  Maybe instead just take b' to
   -- WHNF?  Look carefully at untilB and integral.
   loop ((te,mb) : possOccs') b =
     -- b' `seq`
     (te, map (`pair` y) mb) : loop possOccs' b'
    where
      (y,b') = b `at` te

-- Shortcut when ignoring the given event's data
snapshot_ :: {- Forceable a => -} Event a -> Behavior b -> Event b

e `snapshot_` b = (e `snapshot` b) ==> snd


whenSnap :: Event a -> Behavior b -> (a -> b -> Bool) -> Event a
whenSnap e b pred = (e `snapshot` b `suchThat` uncurry pred) ==> fst

whenE :: Event a -> BoolB -> Event a

-- Snapshot condition, test and discard
e `whenE` b = (e `snapshot` b `suchThat` snd) ==> fst

-- Test out the following alternative:
-- e `whenE` b = whenSnap e b (curry snd)


predicate :: BoolB -> User -> Event ()

-- Check once per update.  See Integral's stepSize for alternative.
predicate b u = check `whenE` b
 where
  check = updateDone u -=> ()
  -- an alternative: check at regular intervals
  -- check = alarmE (startTime u) 0.1

-------- Testing.  Use tstE from Event.hs

-- Another formulation of {Event}e1, only roughly equal, due to timeSince
-- approximation.
be1 t0 = alarmE t0 0.1 `snapshot_` timeSince t0

be2 t0 = e2 t0 `whenE` wholeTime
 where wholeTime = abs (sndB (properFractionB (timeSince t0))) <* 0.01

-- Another formulation. (Try "tstE be2 == tstE be2'")
be2' t0 = (e3 t0 `suchThat` wholeTime) -=> ()
 where wholeTime dt = abs (snd (properFraction dt)) < 0.001

be3 t0 = e2 t0 `snapshot_` sin (timeSince t0)

be3'  t0 = traceE "be2'" TraceAllE  (be2' t0)
be3'' t0 = traceE "be2'" TraceOccsE (be2' t0)

-- I can't test predicate here, because it relies on updateDone, which is
-- not getting fed.  See be4 in Spritify.hs

-- be4 t0 = predicate (timeSince t0 >=* 1) u

