-- Definition of `untilB`
-- 
-- Last modified Sat Sep 07 23:23:12 1996
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
    sampler ts = sampler' ts (b `ats` ts)
    sampler' ts@(t:ts')  ~(val:vals') =
      case occ e t of
        Nothing ->
          --trace ("non-occurrence: " ++ show t ++ " ") $
          val : sampler' ts' vals'
        Just (te,b') ->
          --trace ("occurrence: " ++ show te ++ " for time " ++ show t ++ " ") $
          b' `ats` ts

