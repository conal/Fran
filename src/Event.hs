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
infixl 2 *=>
infixl 0 .|.


infixl 9 `snapshot`


type EventOcc a = Maybe (Time, a)

data Event a = Event (Behavior (EventOcc a))



occ :: Event a -> [Time] -> [EventOcc a]

-- Note: knows the representation of Behavior.

(Event maybeB) `occ` ts = maybeB `ats` ts


(+=>) :: Event a -> (Time -> a -> b) -> Event b
(==>) :: Event a -> (a ->         b) -> Event b
(*=>) :: Event a -> (Time      -> b) -> Event b
(-=>) :: Event a ->               b  -> Event b

-- (+=>) is the general form.  The others are shorthands.
ev ==> f  =  ev +=> const f               -- ignore event time
ev *=> f  =  ev +=> \ t _ -> f t          -- ignore event value
ev -=> x  =  ev ==> const x               -- ignore event time and value

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

    mb@(Just (te,x)) .|.# mb'@(Just (te',x'))
      | te<=te'    =  mb
      | otherwise  =  mb'


-- Literally specified event

constEvent :: Time -> a -> Event a

{-  The following definition yields an error saying that the declared type
    is too general, and the inferred type is
    Ord a => Time -> a -> Event a.  Why?
constEvent t v =
  Event (cond (time >* lift0 t) liftOcc liftNonOcc)
  where
    liftOcc    = lift0 (Just (t,v))
    liftNonOcc = lift0 Nothing
-}

constEvent tEv v =
  Event (Behavior (map (\t -> if t > tEv then occ else Nothing)))
  where
    occ = Just (tEv,v)


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
        Nothing  ->  tryEvB (evg te) ts'



{-

whenEv :: (Time -> Event a) -> Behavior Bool -> Time -> Event a

whenEv evg boolB t0 =
  -- Working here: make a list of the event time/value pairs from evg,
  -- feed the times to boolB, ...

-}


suchThat :: (Time -> Event a) -> (a -> Bool) -> Time -> Event a

suchThat evg pred =
  filterEv evg (\ a -> if pred a then Just a else Nothing)


-- Like predicate (time ==* tVal) (-infinity)
timeIs t = constEvent t ()

-- The event that never happens.  Left- and right-identity for .|.

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
        tsPatched (Just (te,_) : _)    _         = [te]

        loop :: [EventOcc a] -> [b] -> [EventOcc (a,b)]

        loop [Just (te,x)]    [bVal]     = [Just (te, (x,bVal))]

        loop (Nothing : mbs') (_:bVals') = Nothing : loop mbs' bVals'

        loop _ _ = error "snapshot length mismatch"



predicate :: Behavior Bool -> Time -> Event ()

-- Simple implementation, not using interval analysis.

predicate condB t0 =
  Event (Behavior (\ ts -> testCond ts (condB `ats` ts) t0))
  where
    -- When we find a True, assume that the event occurred midway
    -- between the previously checked time and this one.
    testCond (t : _)     (True : _)      tPrev =
      [Just (tPrev + (t-tPrev)/2, ())]
    testCond (t : tRest) (False : bools') _    =
      Nothing : testCond tRest bools' t


-- Testing

tstE (Event mbB) = tstB mbB

e1 = timeIs 3
