-- The "event algebra"
-- 
-- An event represents a (tVal,x) :: (Time, a) pair, and responds the
-- question of whether a given time t > tVal, and if so, what are tVal and
-- x.  (I used to call this notion an "event occurrence" or a "timed
-- value".)
--
-- This implementation represents an (Event a) as a behavior whose values
-- are Maybe (Time,a).
--
-- Last modified Wed Sep 11 16:48:19 1996

module Event where

import Behavior

infixl 2 +=>
infixl 2 ==>
infixl 2 -=>
infixl 0 .|.


infixl 9 `snapshot`


type EventOcc a = Maybe (Time, a)

data Event a = Event (Behavior (EventOcc a))



occ :: Event a -> [Time] -> [EventOcc a]

-- Note: knows the representation of Behavior.

(Event maybeB) `occ` ts = maybeB `ats` ts


(-=>) :: Event a ->               b  -> Event b
(==>) :: Event a -> (a ->         b) -> Event b
(+=>) :: Event a -> (Time -> a -> b) -> Event b

-- (+=>) is the general form.  The others are shorthands.
ev ==> f  =  ev +=> const f               -- ignore event time
ev -=> x  =  ev ==> const x               -- ignore event value

(Event eb) +=> f =
  Event (lift1 g eb)
  where
    g mb  =  mb >>= (return . (\ (te,a) -> (te, f te a)))


-- Earlier of two events

(.|.) :: Event a -> Event a -> Event a

(Event eb) .|. (Event eb') =
  Event (lift2 (.|.#) eb eb')
  where 
    (.|.#) :: EventOcc a -> EventOcc a -> EventOcc a

    Nothing .|.# mb' =  mb'

    mb .|.# Nothing  =  mb

    Just o@(te,x) .|.# Just o'@(te',x') =
      Just (if te<=te' then o else o')


-- Literally specified event

constEvent :: Time -> a -> Event a

constEvent t v = Event (lift0 (Just (t,v)))


filterEv :: (Time -> Event a) -> (a -> Maybe b) -> Time -> Event b

filterEv evg f t0 = 
  Event (Behavior (tryEvB (evg t0)))
  where
    -- tryEvB :: Event a -> [Time] -> [EventOcc b]

    tryEvB (Event (Behavior ebf)) ts  =  filt (ebf ts) ts

    filt (Nothing : mbs') (_ : ts') = Nothing : (filt mbs' ts')

    filt (Just (te,a) : _) (_ : ts')  =
      case f a of
        Just b   ->  [Just (te, b)]
	Nothing	 ->  tryEvB (evg te) ts'


suchThat :: (Time -> Event a) -> (a -> Bool) -> Time -> Event a

suchThat evg pred =
  filterEv evg (\ a -> if pred a then Just a else Nothing)


-- Like predicate (time ==* tVal) (-infinity)
timeIs t = constEvent t ()

-- The event that never happens

never :: Event a

never = Event (lift0 Nothing)



snapshot :: Event a -> Behavior b -> Event (a,b)

(Event eb) `snapshot` b =
  Event (Behavior sf)
  where
    sf ts =
      loop mbs (b `ats` tsPatched mbs ts)
      where
        mbs = eb `ats` ts
    
	tsPatched (Nothing     : mbs') (t : ts') = t : tsPatched mbs' ts'
	tsPatched (Just (te,_) : _)    _	 = [te]

	loop :: [EventOcc a] -> [b] -> [EventOcc (a,b)]

	loop [Just (te,x)]    [bVal]     = [Just (te, (x,bVal))]

	loop (Nothing : mbs') (_:bVals') = Nothing : loop mbs' bVals'

	loop _ _ = error "snapshot length mismatch"
