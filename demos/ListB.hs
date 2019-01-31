-- "List Behaviors".  

module ListB where

import GBehavior
import Event

-- Here b is typically a behavior-like type, such as (Behavior a),
-- or ImageB, I think.

data ListB b
  = NilB
  | ConsB b (ListB b)
  | UntilL (ListB b) (Event (ListB b))

instance  GBehavior b => GBehavior (ListB b)  where

  untilB = UntilL

  NilB `afterTime` te = NilB
  ConsB b l `afterTime` te =
    ConsB (b `afterTime` te) (l `afterTime` te)
  -- Do afterTime as in src/NewBehavior/ImageB.hs, but I think it would be
  -- better to strip off the UntilL if t is after e.
  (imb `UntilL` e) `afterTimeI` t =
    (imb `afterTime` t) `untilB` e ==> (`afterTime` t)


foldrB  :: GBehavior b => (a -> b -> b) -> b -> ListB a -> b

foldrB f z NilB = z

foldr f z (ConsB x xs) = f x (foldrB f z xs)

foldrB f z (l `UntilL` e) =
  foldrB f z  l  `untilB` (e `afterE` z) ==> \ (l', z') ->
  foldrB f z' l'


toListB :: GBehavior b => [b] -> ListB b

toListB = foldr ConsB NilB



-- Generator of transient values.  Occurrences give a value and an
-- extinction event.

type Gen b = Event (b, Event ())

-- Turn a generator into a ListB
genListB :: Gen b -> ListB b
