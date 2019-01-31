-- This module defines the type class of GBehavior, which combines the
-- old GBehavior with type class TimeTransformable. It is put in the
-- stand-alone module to avoid mutual dependence among modules.

module GBehavior where

import BaseTypes
import Event
import Behavior

infixr 1 `untilB`, `untilF`

infixl 3 `afterE`, `afterE_`

------------------------------------------------------------------
-- Class declaraction
------------------------------------------------------------------

class GBehavior bv where
  untilB        :: bv -> Event bv -> bv
  afterTimes    :: bv -> [Time]   -> [bv]
  timeTransform :: bv -> TimeB    -> bv

------------------------------------------------------------------
-- instances
------------------------------------------------------------------

instance GBehavior (Behavior a) where
  untilB        = untilBB
  afterTimes	= afterTimesB
  timeTransform = timeTransformB

instance (GBehavior bv1, GBehavior bv2) => GBehavior (bv1,bv2) where
 (bv1, bv2) `untilB` e    =
   (bv1 `untilB` e ==> fst, bv2 `untilB` e ==> snd)
 (bv1, bv2) `afterTimes` ts = zip (bv1 `afterTimes` ts) (bv2 `afterTimes` ts)
 timeTransform = error "timeTransform for GBehavior (,) not available"

instance (GBehavior bv1, GBehavior bv2, GBehavior bv3)
      => GBehavior (bv1,bv2,bv3) where
 (bv1, bv2, bv3) `untilB` e    =
   ( bv1 `untilB` e ==> \(bv1',_,_) -> bv1'
   , bv2 `untilB` e ==> \(_,bv2',_) -> bv2'
   , bv3 `untilB` e ==> \(_,_,bv3') -> bv3'
   )
 (bv1, bv2, bv3) `afterTimes` ts =
   zip3 (bv1 `afterTimes` ts) (bv2 `afterTimes` ts) (bv3 `afterTimes` ts)
 timeTransform = error "timeTransform for GBehavior (,,) not available"

-- I thought this would be useful.  However, the event should really be a
-- function of bv, which breaks the mold.
instance (GBehavior bv, GBehavior bv') => GBehavior (bv -> bv') where
 untilB f e = \ bv -> f bv `untilB` e `afterE` bv ==> uncurry ($)
 afterTimes = error "afterTimes for GBehavior (->) not available"
 timeTransform = error "timeTransform for GBehavior (->) not available"

-- Instead, use this guy.  Should there be one version for GBehavior a
-- that does the afterE and one for non-GBehavior a that doesn't??
untilF :: (GBehavior bv, GBehavior a)
       => (a -> bv) -> (a -> Event (a -> bv)) -> (a -> bv)
untilF f e = \ a -> f a `untilB` e a `afterE` a ==> uncurry ($)

instance (GBehavior bv) => GBehavior (Maybe bv) where
 -- There can't be an untilB, since there's no way to change between
 -- Nothing and Just bv.
 untilB             = error "Sorry -- no untilB for (Maybe bv)"
 Nothing `afterTimes` ts = map (const Nothing) ts
 Just bv `afterTimes` ts = map Just (bv `afterTimes` ts)
 timeTransform = error "timeTransform for GBehavior (Maybe) not available"

instance GBehavior (Event a) where
  untilB	= untilBE
  afterTimes    = afterTimesE
  timeTransform = error "timeTransform for GBehavior (Event) not available"


------------------------------------------------------------------
-- anything from modules Behavior and Event and/BehaviorEvent that
-- involves GBehavior and/or TimeTransformable
------------------------------------------------------------------

afterE :: GBehavior bv => Event a -> bv -> Event (a, bv)
Event possOccs `afterE` bv =
  -- Take the remainders of bv at each time in possOccs
  --trace ("afterE: times are " ++ show evTimes ++ "\n") $
  Event (loop possOccs (bv `afterTimes` evTimes))
 where
   evTimes = map fst possOccs
   loop [] _ = []

   -- Pair up the event data, if any, with the behavior residual.
   -- This lazy pattern is necessary, so that the bv doesn't get "sampled"
   -- too soon, which is especially important if it involves the user.
   -- But, it does create a space leak and later latency from
   -- unevaluated bvAfter's, heads and tails.  The seq eliminates that
   -- leak, by making sure that the bvAfters cons cell gets created very
   -- soon.
   loop ((te,mb) : possOccs') ~bvAfters@(bvAfter : bvAfters') =
     --trace "afterE\n" $
     -- The seq below is to help avoid the space leak.  Unfortunately, it
     -- then becomes too strict.  See seqD9 in Spritify.hs
     (te, map (`pair` bvAfter) mb) :
     (bvAfters `seq` loop possOccs' bvAfters')

afterE_ :: GBehavior bv => Event a -> bv -> Event bv
e `afterE_` b  = e `afterE` b ==> snd

-- A switcher for piecewise-constant behaviors
stepper :: a -> Event a -> Behavior a
stepper x0 e = switcher (constantB x0) (e ==> constantB)

-- Assemble a behavior piecewise from an initial one and an event
switcher :: GBehavior bv => bv -> Event bv -> bv
switcher b0 e = b0 `untilB` repeatE e

repeatE :: GBehavior bv => Event bv -> Event bv
repeatE e = withRestE e ==> uncurry switcher

-- Accumulate using f, but age the accumulator
accumB :: GBehavior bv => (bv -> b -> bv) -> bv -> Event b -> bv
accumB f soFar e =
  soFar `untilB` withRestE e `afterE` soFar ==> \ ((x,e'),soFar') ->
  accumB f (f soFar' x) e'

-- Here's a much simpler accumB definition, but it has the problem that it
-- does't "age" soFar.  See the comment in scanlE.
-- 
-- accumB f soFar e = switcher soFar (scanlE f soFar e)
--
-- Look for a better way to define accumB.

