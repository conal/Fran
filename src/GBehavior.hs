-- This module defines the type class of GBehavior, which combines the
-- old GBehavior with type class TimeTransformable. It is put in the
-- stand-alone module to avoid mutual dependence among modules.

module GBehavior where

import BaseTypes
import Event
import Behavior
import IOExts (trace)

infixr 1 `untilB`, `untilF`

infixl 3 `afterE`, `afterE_`

------------------------------------------------------------------
-- Class declaraction
------------------------------------------------------------------

class GBehavior bv where
  untilB        :: bv -> Event bv -> bv
  afterTimes    :: bv -> [Time]   -> [bv]
  timeTransform :: bv -> TimeB    -> bv
  condBUnOpt    :: BoolB -> bv    -> bv    -> bv

condB, ifB :: GBehavior bv => BoolB -> bv -> bv -> bv
condB (Behavior (ConstantB True ) _) imb _    = imb
condB (Behavior (ConstantB False) _) _   imb' = imb'
condB c imb imb' = condBUnOpt c imb imb'

-- Synonym
ifB = condB

------------------------------------------------------------------
-- instances
------------------------------------------------------------------

instance GBehavior (Behavior a) where
  untilB        = untilBB
  afterTimes	= afterTimesB
  timeTransform = timeTransformB
  condBUnOpt    = condBB

instance (GBehavior bv1, GBehavior bv2) => GBehavior (bv1,bv2) where
 (bv1, bv2) `untilB` e    =
   (bv1 `untilB` e ==> fst, bv2 `untilB` e ==> snd)
 (bv1, bv2) `afterTimes` ts = zip (bv1 `afterTimes` ts) (bv2 `afterTimes` ts)
 timeTransform = error "timeTransform for GBehavior (,) not available"
 condBUnOpt c (thenBv1, thenBv2) (elseBv1, elseBv2) =
   (condBUnOpt c thenBv1 elseBv1, condBUnOpt c thenBv2 elseBv2)

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
 condBUnOpt c (thenBv1, thenBv2, thenBv3) (elseBv1, elseBv2, elseBv3) =
   (condBUnOpt c thenBv1 elseBv1,
    condBUnOpt c thenBv2 elseBv2,
    condBUnOpt c thenBv3 elseBv3)

-- I thought this would be useful.  However, the event should really be a
-- function of bv, which breaks the mold.
instance (GBehavior bv, GBehavior bv') => GBehavior (bv -> bv') where
 untilB f e = \ bv -> f bv `untilB` e `afterE` bv ==> uncurry ($)
 afterTimes = error "afterTimes for GBehavior (->) not available"
 timeTransform = error "timeTransform for GBehavior (->) not available"
 condBUnOpt c thenF elseF = \ bv -> condBUnOpt c (thenF bv) (elseF bv)

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
 condBUnOpt    = error "condBUnOpt for GBehavior (Maybe) not available"

instance GBehavior (Event a) where
  untilB	= untilBE
  afterTimes    = afterTimesE
  timeTransform = error "timeTransform for GBehavior (Event) not available"
  condBUnOpt = error "condBUnOpt for GBehavior (Event) not available"
  -- The following seems a fine definition.  (Thanks to Robert Ennals.)
  -- Unfortunately, I have a problem with recursive modules, involving
  -- GBehavior, BehaviorEvent, and User.
  --condBUnOpt c eThen eElse = (eThen `whenE` c) .|. (eElse `whenE` notB c)


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

-- Map a function over a switching behavior
mapSwitcher :: GBehavior b => (a -> b) -> a -> Event a -> b
mapSwitcher f x0 e = switcher (f x0) (e ==> f)

-- A switcher for piecewise-constant behaviors
stepper :: a -> Event a -> Behavior a
stepper x0 e = mapSwitcher constantB x0 e

-- Assemble a behavior piecewise from an initial one and an event
switcher :: GBehavior bv => bv -> Event bv -> bv
switcher b0 e = b0 `untilB` repeatE e

repeatE :: GBehavior bv => Event bv -> Event bv
repeatE e = withRestE e ==> uncurry switcher

-- Accumulate using op, but age the accumulator
accumB' :: GBehavior bv => (bv -> b -> bv) -> bv -> Event b -> bv
accumB' op soFar e =
  soFar `untilB` withRestE e `afterE` soFar ==> \ ((x,e'),soFar') ->
  accumB' op (soFar' `op` x) e'

-- Here's a much simpler accumB definition, but it has the problem that it
-- does't "age" soFar.  See the comment in scanlE.
-- 
-- accumB f soFar e = switcher soFar (scanlE f soFar e)
--
-- Look for a better way to define accumB.

-- Alternative to accumB that avoids the need for afterE.  Less
-- polymorphic and slightly different semantics, agrees when the op is
-- associative and ident is an identity.
accumB :: GBehavior bv => (bv -> bv -> bv) -> bv -> Event bv -> bv
accumB op ident = loop
 where
   loop e = ident `untilB` withRestE e ==> \ (b',e') ->
            b' `op` loop e'

-- Yet another way to avoid afterE
switchAccum :: GBehavior bv => bv -> Event (bv -> bv) -> bv
switchAccum nothing = loop
 where
   loop e = nothing `untilB` withRestE e ==> \ (augment, e') ->
            augment (loop e')

-- A similar operation but for piecewise-constant behaviors:

stepAccum :: a -> Event (a -> a) -> Behavior a
stepAccum x0 change = stepper x0 (accumE x0 change)

-- Here's an almost equivalent alternative.  The only difference comes
-- from simultaneous pushes.  In the second version, the "later" of two
-- simultaneous pushes will snapshot b at the time of both occurrences,
-- when neither has taken effect.

-- stepAccum x0 change = b
--  where
--    b = stepper x0 (change `snapshot` b ==> uncurry ($))



-- Useful for spritification
mbTTrans :: GBehavior bv => bv -> Maybe TimeB -> bv
bv `mbTTrans` Nothing = bv
bv `mbTTrans` Just tt = --trace "Really time-transforming\n" $
                        bv `timeTransform` tt
