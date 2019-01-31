-- Event combinators that involve events.  Separated out from Event.hs to
-- avoid a mutual module recursion.
--
-- Last modified Mon Nov 03 13:18:29 1997

module BehaviorEvent where

import BaseTypes (pair)
import Event
import User
import Behavior
import Maybe (isJust)

import Trace

infixl 3 `snapshot`, `snapshot_`, `whenE`

-- The event "e `snapshot` b" occurs at the time te when e occurs.  Its
-- value is e's value together with a snapshot of b at te.  A counterpart
-- to afterE.

snapshot :: Event a -> Behavior b -> Event (a, b)

Event possOccs `snapshot` b =
  Event (loop possOccs (b `ats` (map fst possOccs)))
 where
   loop [] _ = []

   -- Pair up the event data, if any, with the behavior sample.
   -- This lazy pattern is necessary, so that b doesn't get sampled
   -- too soon, which is especially important in self-reactive situations.
   -- See comments about the lazy pattern and "seq" for afterE in Event.hs.
   loop ((te,mb) : possOccs') ~xs@(x:xs') =
     (te, map (`pair` x) mb) : (xs `seq` loop possOccs' xs')


{-

-- This next version wedges iTst9 and iTst10 in Spritify.hs
e@(Event possOccs) `snapshot` b =
  Event (loop possOccs (b `ats` occTimes e))
 where
   loop [] _ = []

   loop ((te,mb) : possOccs') ~xs@(x:xs') =
     (te, mbSnap) : loop possOccs' xs'
    where
      (mbSnap, xs') = case mb of
                        Nothing -> (Nothing   , xs )
                        Just y  -> (Just (y,x), xs')

--occTimes :: Event a -> [Time]

occTimes (Event possOccs) = loop possOccs
 where
   loop ((_, Nothing) : possOccs') = loop possOccs'
   loop ((te, _) : possOccs')      = te : loop possOccs'
-}


-- Shortcut when ignoring the given event's data
snapshot_ :: Event a -> Behavior b -> Event b

e `snapshot_` b = e `snapshot` b ==> snd


whenSnap :: Event a -> Behavior b -> (a -> b -> Bool) -> Event a
whenSnap e b pred = e `snapshot` b `suchThat` uncurry pred ==> fst

whenE :: Event a -> BoolB -> Event a

-- Snapshot condition, test and discard
-- e `whenE` b = (e `snapshot` b `suchThat` snd) ==> fst

-- Test out the following alternative:
e `whenE` b = whenSnap e b (curry snd)


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
be2' t0 = e3 t0 `suchThat` wholeTime -=> ()
 where wholeTime dt = abs (snd (properFraction dt)) < 0.001

be3 t0 = e2 t0 `snapshot_` sin (timeSince t0)

be3'  t0 = traceE "be2'" TraceAllE  (be2' t0)
be3'' t0 = traceE "be2'" TraceOccsE (be2' t0)

-- I can't test predicate here, because it relies on updateDone, which is
-- not getting fed.  See be4 in Spritify.hs

-- be4 t0 = predicate (timeSince t0 >=* 1) u
