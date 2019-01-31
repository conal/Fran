-- Primitives for non-reactive behaviors, based on stacks of infinite
-- "behavior trees" of sample values.  This version doesn't do interval
-- analysis.
-- 
-- Last modified Tue Aug 05 09:43:40 1997
--
-- To do:
--
--  + untilB: Try to shift work from sampling to construction.
--  + Finish timeTransform.  See notes.
--  + Make sampling do no cons'ing.  See note in "at" definition.

module BStack where

import BaseTypes
import Event
import Trace

-- A btree (pronounced "bee tree", for "behavior tree") represents a
-- behavior over an interval.  It has a midpoint value and two sub-btrees,
-- with the interval and its start and end values being known from
-- context.

data BTree a = BTree a (BTree a) (BTree a)

-- Primitive behavior representation as a stack of btrees, each with a
-- start time.

newtype BPrim a = BStack [(Time, BTree a)]

instance Show (BPrim a) where
  showsPrec p (BStack _)    = showString "<<BStack>>"

instance GBehavior (BPrim a) where
  untilB          = error "BStack: no untilB on BPrim."
  b `afterTime` t = snd (b `at` t)
  startTime (BStack ((t0,_):_)) = t0

-- The abstract interface: sample a behavior with a time to get a new time
-- and behavior.

at :: BPrim a -> Time -> (a, BPrim a)

BStack stack `at` t = --trace ("Sampling BStack at " ++ show t ++ "\n") $
                      sampleStack stack
 where
  sampleStack stack @((t0, BTree aMid left right) :
              stack'@((t1, _) : _))
{-
    -- Time before info: should never happen.  Maybe change to "t <= t0", but
    -- we occasionally need to sample at start time, as in integration and
    -- spritification.
    | t < t0     =  error $ "{Behavior} at: time too small. " ++
                    show t ++ " < " ++ show t0
-}
    -- First btree irrelevant: skip over
    | t > t1     =  --trace "s" $
                    sampleStack stack'
    -- First interval too long: sub-divide.  Idea: stash this remainder
    -- list in the BTree to avoid re-creating it.  Ideally, sampling should
    -- not cons.  (Might be okay as is.)  Take special care for machine
    -- epsilon, especially with Hugs, whose "doubles" are really single
    -- precision.  Only subdivide if real progress is made for both halves.

    | dt > minStep && tMid - t0 < dt && t1 - tMid < dt =
        (if (t < t0) then trace "{Behavior} at: time too small." else id)$
        --trace ("d" {- ++ show (t0,t1,dt,tMid-t0) -}) $
        --tMid `seq` 
        sampleStack ((t0, left) : (tMid, right) : stack')
    -- Close enough: return value and remainder.  Idea: stash this pair in
    -- the BTree to avoid re-creating it.

    | otherwise =  --trace ". " $
                   (aMid, BStack stack)
   where
     dt   = t1 - t0
     tMid = t0 + dt / 2

  -- Min step size for sampling.  Experiment with this one.
  minStep = 1.0e-3

-- Our bstacks need start times, so no plain old "time"
-- However, this is a really bad implementation of timeSince, since it
-- requires a lot of traversal work instead of a simple computation.

timeSince :: Time -> BPrim Time
timeSince t0 = BStack (trees t0 1)
 where
   trees t1 dt = -- t1 `seq` dt `seq`
                 (t1, tree t1 dt) : trees (t1+dt) (2 * dt)

   tree t1 dt = -- t1 `seq` dt `seq`
                BTree (tMid-t0) (tree t1 halfDt) (tree tMid halfDt)
    where
      halfDt = dt / 2
      tMid   = t1 + halfDt

-- Other cases

fb `apply` xb = BStack (stacks tStart fb xb 1)
 where
   tStart = startTime fb `max` startTime xb

   stacks tStart fb xb dt =
     -- I think we have to force these so they cannot build up into huge
     -- postponed computations.  Experiment with and without the `seq`s.
     -- Try with all, none, just tNext, and just fb&xb.
     --tNext `seq` fb `seq` xb `seq`
     (tStart, tree tStart fb xb dt) :
     stacks tNext (fb `afterTime` tNext) (xb `afterTime` tNext) (2 * dt)
    where
      tNext = tStart + dt

   tree tStart fb xb dt =
     --tMid `seq` fb `seq` xb `seq`       -- see previous comment
     BTree (fMid xMid)
           (tree tStart fb  xb  halfDt)
           (tree tMid   fb' xb' halfDt)
    where
      halfDt      = dt / 2
      tMid        = tStart + halfDt
      (fMid, fb') = fb `at` tMid
      (xMid, xb') = xb `at` tMid


applyConstant :: (a -> b) -> BPrim a -> BPrim b
f `applyConstant` BStack stack = BStack (map appStack stack)
 where
   appStack (tStart, tree) = (tStart, liftBTree tree)

   liftBTree (BTree aMid left right) =
      BTree (f aMid) (liftBTree left) (liftBTree right)

applyToConstant :: BPrim (a -> b) -> a -> BPrim b
BStack stack `applyToConstant` a = BStack (map appStack stack)
 where
   appStack (tStart, tree) = (tStart, liftBTree tree)

   liftBTree (BTree fMid left right) =
      BTree (fMid a) (liftBTree left) (liftBTree right)
