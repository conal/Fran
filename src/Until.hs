-- Definition of `untilB`
-- 
-- Last modified Mon Sep 16 21:52:51 1996
--
-- Warning: the implementation of untilB assumes that the given time
-- stream does not go backwards across events.  We cannot guarantee this
-- property, so be careful!

module Until where

import Event
import Behavior 

infixr 1 `untilB`

class GBehavior bv where
  untilB :: bv -> Event bv -> bv

instance GBehavior (Behavior a) where
  untilB = untilBB


untilBB :: Behavior a -> Event (Behavior a) -> Behavior a

b `untilBB` e =
  Behavior sampler
  where
    sampler ts = sampler' ts (b `ats` ts) (e `occ` ts)

    sampler' ts@(t:ts')  ~(val:vals') (mb:mbs') =
      case mb of
        Nothing ->
          --trace ("non-occurrence: " ++ show t ++ " ") $
          val : sampler' ts' vals' mbs'
        Just (te,b') ->
          --trace ("occurrence: " ++ show te ++ " for time " ++ show t ++ " ") $
          b' `ats` ts



-- Testing (use tstB)

u1 = 0 `untilB` timeIs 1.5 -=> 1  :: Behavior Int

