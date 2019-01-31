-- Reactivity, via "untilB" behaviors
-- 
-- Warning: the implementation of untilB assumes that the given time
-- stream does not go backwards across events.  We cannot guarantee this
-- property, so be careful!

module Until where

import Event
import Fuzzy
import Behavior 

infixr 1 `untilB`

-- The "untilB" operator belongs to a class of "generalized" behaviors:

class GBehavior bv where
  untilB :: bv -> Event bv -> bv

instance GBehavior (Behavior a) where
  untilB = untilBB


untilBB :: Behavior a -> Event (Behavior a) -> Behavior a

b `untilBB` e  =  b `untilCached` (cacheEvent e)


-- This one uses caching, statefully implemented events

b `untilCached` cachedEv = Behavior sample isample
 where
  sample t =
    --trace ("untilCached: time == " ++ show t ++ "\n") $
    case  cachedEv t  of
      -- If e doesn't occur before t, yield b's value at t, with
      -- a reconstructed untilBB behavior, based on possible changes
      -- to b and e.
      Nothing        ->  (x,  b' `untilCached` cachedEv)
			  where (x, b') = b `at` t
      -- Otherwise
      Just (_,bAfterE)  ->  bAfterE `at` t
  isample = noI "untilB"

{-

-- This one uses functional events, but caches, so that separate samplings
-- of the same untilBB behavior don't re-detect the event.  When the
-- redundant sampling problem is solved, we should be able to eliminate
-- the use of cacheEvent.

b `untilBB` e = b `untilCached` (cacheEvent e)
 where
  b `untilCached` e =
    Behavior $ \ t ->
    case  e `occ` t  of
      -- If e doesn't occur before t, yield b's value at t, with
      -- a reconstructed untilBB behavior, based on possible changes
      -- to b and e.
      NonOcc e'        ->  (x,  b' `untilBB` e')
                             where (x, b') = b `at` t
      -- Otherwise
      Occ _ bAfterE  ->  bAfterE `at` t

-}
