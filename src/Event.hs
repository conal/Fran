-- The "event algebra".
-- 
-- Last modified Sun Nov 10 17:20:06 1996


module Event where

import Fuzzy (Upto, TimeI)
import Behavior (Time, Behavior, at, during, BoolB)
import IORef(Ref,newRef,getRef,setRef)
import IOExtensions(unsafePerformIO)
import Utils(trace)
import Force

infixl 9 `snapshot`

infixl 3 +=>
infixl 3 ==>
infixl 3 -=>
infixl 3 *=>
infixl 2 .|.

infixl 1 +>>=   -- generalization of >>=


-- An event maps a time to either a time/value pair, in case of an
-- occurrence, or a new event, if no occurrence.

data Event a = Event (Time -> PossibleOcc a)

data  PossibleOcc a  =  Occ Time  a | NonOcc (Event a)  -- deriving Text

-- The abstract interface:

occ :: Event a -> Time -> PossibleOcc a

occ (Event sample) = sample


-- Event handling:


-- Note that an event happens no earlier than a given time.  Useful for
-- doing some work to determine a lower bound for an event time.  Laziness
-- postpones determination of the event time for a while.

timeIsAtLeast :: Time -> Event a -> Event a

timeIsAtLeast lowerBound ev = ev'
 where
  ev'       =  Event sample

  sample t  =  if t <= lowerBound then
                 NonOcc ev'
               else ev `occ` t


-- The event "ev +=> f" occurs exactly when ev occurs.  Its value is
-- (f te x), where te and x are the time and value of ev's occurrence.

(+=>) :: Event a -> (Time -> a -> b) -> Event b

ev +=> f =
 Event $ \ t ->
 case  ev `occ` t  of
   Occ te x   ->  Occ te (f te x)
   NonOcc ev' ->  NonOcc (ev' +=> f)


-- The event e .|. e' corresponds to the earlier of e and e', prefering
-- e if they occur simultaneously.

(.|.) :: Event a -> Event a -> Event a

ev .|. ev'  =
  Event $ \ t ->
  case  (ev `occ` t, ev' `occ` t)  of 
    (NonOcc evNew, NonOcc evNew' )  -> NonOcc (evNew .|. evNew')
    (o           , NonOcc _      )  -> o
    (NonOcc _    , o'            )  -> o'
    (o@(Occ te _), o'@(Occ te' _))  -> if te <= te' then o else o'


-- joinEvent e is the event that occurs when e' occurs, where e' is the
-- event data part of e.  (Should be time adjusted to be no earlier than
-- e.)  This is "join" for the Event monad.  I use it to define +>>=
-- below, which is used to define >>=.  Look into turning this around, so
-- that I can just use "join", as defined in StdLib/Monad.hs.

joinEvent :: Event (Event a) -> Event a

joinEvent ev =
 Event $ \ t ->
 case ev `occ` t of
   NonOcc newEv  -> NonOcc (joinEvent newEv)
   Occ te ev'    ->
     --trace ("joinEvent: found inner occurrence at " ++ show te ++ "\n") $
     ev' `occ` t

-- A "constant event" that has a given time and value

constEvent :: Time -> a -> Event a

constEvent te x = constEv
  where constEv = Event sample
        sample t
          | te <= t   =  Occ te x
          | otherwise =  NonOcc constEv


-- An event that never happens

neverEvent :: Event a

neverEvent = Event (\ t -> NonOcc neverEvent)


-- The event "e `snapshot` b" occurs at the time te when e occurs.  Its
-- value is e's value together with a snapshot of b at te.

snapshot :: (Forceable b) => Event a -> Behavior b -> Event (a,b)

ev `snapshot` b =
  Event $ \ t ->
  case  ev `occ` t  of
    Occ te x   ->  Occ te (x,y)
                     where (y, _) = b `at` te

    -- We force the evaluation of y below to aviod a problem with snapshot
    -- of the mouse.
    NonOcc ev' ->  force y `seq` 
                   NonOcc (ev' `snapshot` b')                
                     -- move b along
                     where (y, b') = b `at` t


-- Earliest event time
minPossibleTime = -(10^20) :: Time


-- This one does caching.  See Until.hs for a use.

cacheEvent :: Event a -> (Time -> Maybe (Time, a))

cacheEvent ev = cachedEv
 where
{- 
  -- Equivalent code with no caching - use for debugging
  uncachedEv t =
    case  ev `occ` t  of
    Occ te x    -> Just (te,x)
    NonOcc ev'' -> Nothing
-}
  cachedEv =
   unsafePerformIO (
    newRef (Right (minPossibleTime, ev))  >>= \ cacheVar ->
    return $ \ t ->
    unsafePerformIO (
     getRef cacheVar      >>= \ cacheVal ->
     case cacheVal of
       -- Already occurred
       Left pair@(te, _) ->
         return (if  t <= te then Nothing else Just pair)
       -- Not yet occurred
       Right (lowerBound, ev') ->
         if t <= lowerBound then
           return Nothing
         else
           case  ev' `occ` t  of
             Occ te x    ->
               setRef cacheVar (Left (te, x)) >>
               return (Just (te,x))
             NonOcc ev'' ->
               setRef cacheVar (Right (t,  ev'')) >>
               return Nothing ) )



-- Non-primitives

(==>) :: Event a -> (a ->         b) -> Event b
(*=>) :: Event a -> (Time      -> b) -> Event b
(-=>) :: Event a ->               b  -> Event b

ev ==> f  =  ev +=> const f               -- ignore event time
ev *=> f  =  ev +=> \ t _ -> f t          -- ignore event value
ev -=> x  =  ev ==> const x               -- ignore event time and value



-- "e +>>= f" is the event e' = f te x, where te and x are the time and
-- value from e.  Note: if e' occurs earlier than e, it will still not be
-- detected until after e.  Thus occurrence time should probably be
-- tweaked to be at least e's.

(+>>=) :: Event a -> (Time -> a -> Event b) -> Event b

-- Use +=> to make an event-valued event, and joinEvent to flatten.

ev +>>= f  =  joinEvent (ev +=> f)


-- First time cond is true after t0

predicate, predicate' :: Behavior Bool -> Time -> Event ()


-- Old version: sample the condition at regular intervals.

predicate' cond t0 =
  --trace ("predicate: time == " ++ show t0 ++ ", condVal == " ++ show condVal ++ "\n") $
  -- We know that the predicate event happens no sooner than t0.  Saying
  -- so here will prevent cond from getting sampled at t0 or before, which
  -- is necessary for "self reactive" (or systems of mutually reactive)
  -- behaviors.
  timeIsAtLeast t0 $
    if condVal then
      timeIs t0
    else
      --trace ("predicate: trying time " ++ show t0Next ++ "\n")$
      timeIs t0Next          >>    -- using monad interpretation
      predicate' cond' t0Next
  where
    (condVal, cond') = cond `at` t0
    t0Next = t0 + epsilon
    epsilon = 0.05 :: Time


-- New predicate, using interval analysis.

predicate cond t0 = predAfter cond t0 1
  where
    predAfter cond t0 width =
      predIn    cond  (t0 `Upto` t0+width) $ \ cond' ->
      predAfter cond' (t0+width) (2*width)

    predIn :: BoolB -> TimeI -> (BoolB -> Event ()) -> Event ()

    predIn cond iv tryNext =
      case valI of
          -- no occurrence
        False `Upto` False    ->
          -- Note lower bound and try the next condition.
          timeIsAtLeast hi (tryNext cond')
        False `Upto` True     ->
          if hi-mid <= eventEpsilon then
              constEvent mid ()
          else
              predIn cond    (lo `Upto` mid) $ \ midCond ->
              predIn midCond (mid `Upto` hi) tryNext
        True `Upto` True -> constEvent lo ()
      where
        lo `Upto` hi   =  iv
        mid            =  (hi+lo)/2
        ivLeftTrimmed  =  lo + leftSkipWidth `Upto` hi
        (valI,cond')   =  cond `during` ivLeftTrimmed

-- Interval size limit for temporal subdivision.
eventEpsilon = 0.001 :: Time

-- Hack: simulate left-open-ness via a small increment.  Look for a rigorous
-- alternative.

leftSkipWidth = 0.0001 :: Time


-- Filter out events whose data doesn't satisfy a condition.  Take a
-- function that generates events from start time.  If the first generated
-- event doesn't satisfy the condition, try another and repeat.

suchThat :: (Time -> Event a) -> (a -> Bool) -> Time -> Event a

suchThat evg pred =
  filterEv evg (\a -> if pred a then Just a else Nothing) 


-- Generalization of suchThat, replacing a condition by a maybe value.

filterEv :: (Time -> Event a) -> (a -> Maybe b) -> Time -> Event b

filterEv evg f t0 = loop t0
 where
  -- Simple sequential recursion
  loop t0 =
    -- trace ("filtering at " ++ show t0 ++ "\n") $
    evg t0           +>>= \ tVal a ->
    -- trace ("prefiltered event at time " ++ show tVal ++ "\n") $
    case  f a  of
      Just b   ->  constEvent tVal b
      Nothing  ->  loop tVal


-- Like predicate (time ==* tVal) minPossibleTime

timeIs :: Time -> Event ()

timeIs tVal  =  constEvent tVal ()



-- Type class instances.  Monadic stuff.

instance Functor Event where
  map = flip (==>)

instance Monad Event where
 ev >>= f   =  ev +>>= const f
 return v   =  constEvent minPossibleTime v

instance MonadZero Event where
 zero  = neverEvent

instance MonadPlus Event where
 (++)  = (.|.)


{- Delete the rest

-- It's also useful to filter on whether a boolean behavior is true at the
-- occurrence of an event.

whenEv :: (Time -> Event a) -> Behavior Bool -> Time -> Event a

whenEv evg condB t0 = tryEv (evg t0) condB
  where
    tryEv ev condB t =
      case  ev t  of
        NonOcc ev'   ->  NonOcc (tryEv ev' condB')
                         where
                          -- move condB along
                          (_, condB') = condB `at` t
        o@(Occ te x) -> if condVal then
                          NonOcc (whenEv evg condB' te)
                        else o
                        where
                         (condVal, condB') = condB `at` te


predicate condB t0 = Event sample
 where
  sample t = if t <= t0 then NonOcc event
             else if condVal then Occ t ()
             else NonOcc (predicate condB' t)
             where
               (condVal, condB') = condB `at` t

-}
