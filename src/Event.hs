-- Event modeling.
-- 
-- Last modified Sat Sep 07 23:23:09 1996
--
-- 
-- An event represents a (tVal,x) :: (Time, a) pair, and responds the
-- question of whether a given time t > tVal, and if so, what are tVal and
-- x.  (I used to call this notion an "event occurrence" or a "timed
-- value".)
--
-- This version of Event makes the representation of Events abstract.
-- Internally, an Event value uses caching to avoid having to recompute
-- its value once it becomes fixed.
--
-- This version doesn't support predicate events

module Event 

        (
         Event,         -- abstract
         EventOcc(..),
         (-=>),         -- :: Event a -> b -> Event b
         (==>),         -- :: Event a -> (a -> b) -> Event b
         (+=>),         -- :: Event a -> (Time -> a -> b) -> Event b
         (.|.),         -- :: Event a -> Event a -> Event a
         (>=>),         -- :: Event a -> (a -> Event b) -> Event b
         (+>>=),        -- :: Event a -> (Time -> a -> Event a) -> Event b
         timeIs,        -- :: Time -> Event ()
         -- predicate,     -- :: Behavior Bool -> Time -> Event ()
         -- snapshot,   -- :: Behavior a -> Event () -> Event a
         occ,           -- :: Event a -> Time -> Maybe (Time, a)
         mkEvt,
         filterEv,      -- :: (Time -> Event a) -> (a -> Maybe b) -> Time -> Event b
         suchThat,      -- :: (Time -> Event a) -> (a -> Bool) -> Time -> Event a
         -- traceEvent, -- :: SolveTrace -> Event ()
         constEvent,    -- :: Time -> a -> Event a
         -- SolveTrace(..)
        ) where

import Prelude hiding (MutVar, newVar,readVar,writeVar)
import Behavior
-- import Solve
import MutVar

{-
 An Event will at some time t become bound to some value a,
 after that, the Event value stays constant. To avoid recomputing
 this value after it becomes fixed, this representation of Event
 uses caching to store this eventual value. 

 Before the Event fires, the implementation keeps track of the latest time
 we know the Event has not yet occurred - this is to avoid having to reply
 the Time to Event value function unnecessarily.
-}

data Event a
 = Event 
     (MutVar 
        (Either 
             {-
                Time is the latest time we know
                that Event has not fired.
                The function value is the event function.
             -}
           (Time,Time -> Maybe (Time, a))
             {- Value event got bound to (eventually) -}
           (Time,a)))


type EventOcc a = Maybe (Time, a)


--OLD: type Event a  = Time -> EventOcc a

infixl 2 +=>
infixl 2 ==>
infixl 2 -=>
infixl 0 .|.

-- Checking Event occurrence

occ :: Event a -> Time -> EventOcc a

occ (Event cached) t =
 unsafePerformIO (
   readVar cached >>= \ v ->
   case v of
    Left (tlast,f) ->
        if t <= tlast then
           return Nothing
        else
           case f t of
             Nothing -> 
--               putStrLn "Event not occurring" >>
                 writeVar cached (Left (t,f)) >>
                 return Nothing
             mb@(Just evOcc)  ->
--               putStrLn "Event occurred" >>
                 writeVar cached (Right evOcc) >>
                 return mb
    Right o@(tVal,_) -> 
--      putStrLn "Event happened" >>
        if t <= tVal then
           return Nothing
        else
           return (Just o))

-- Turning an event occurrence function into the abstract Event type,
-- giving it an initial sampling time.

mkEvt :: Time -> (Time -> Maybe (Time,a)) -> Event a

mkEvt tstart f = 
 unsafePerformIO (
    newVar (Left (tstart,f)) >>= \ cached ->
    return (Event cached))

startingTime :: Event a -> Time
startingTime (Event cached) =
 unsafePerformIO (
  readVar cached >>= \ v ->
  case v of
    Left  (t,_) -> return t
    Right (t,_) -> return t)


-- Parallel composition, if both events fire at the same time,
-- pick the leftmost one.
(.|.) :: Event a -> Event a -> Event a

(ev .|. ev') = mkEvt (min (startingTime ev) (startingTime ev')) (sample)
  where
   sample t =
    case (occ ev t, occ ev' t) of
      (Nothing, Nothing)  ->  Nothing
      (Nothing, mb')      ->  mb'
      (mb,      Nothing)  ->  mb
      (mb@(Just (tVal,x)), mb'@(Just (tVal',x'))) ->
        if tVal<=tVal' then mb else mb'

-- Note that the implementation of .|. rechecks the structure of ev t
-- and ev' t.  However, as soon as any case but the first is taken, we
-- know that the answer will always be the same.  Consider destructively
-- replacing the query function produced by (ev .|. ev').

(-=>) :: Event a ->               b  -> Event b
(==>) :: Event a -> (a ->         b) -> Event b
(+=>) :: Event a -> (Time -> a -> b) -> Event b


-- (+=>) is the general form.  The others are shorthands.
ev ==> f  =  ev +=> const f               -- ignore event time
ev -=> x  =  ev ==> const x               -- ignore event value

-- Apply f to event time and value
(ev +=> f) =
 mkEvt (startingTime ev)
  (\ t ->
    case occ ev t of
      Nothing       -> Nothing
      Just (tVal,x) -> Just (tVal, f tVal x))

(ev >=> fev) =
 mkEvt (startingTime ev)
  (\ t ->
     case occ ev t of
       Nothing       -> Nothing
       Just (tVal,x) -> occ (fev x) tVal)

(ev +>>= fev) =
 mkEvt (startingTime ev)
  (\ t ->
     case occ ev t of
       Nothing       -> Nothing
       Just (tVal,x) -> occ (fev tVal x) tVal)

-- Instances
instance Functor Event where
  map = flip (==>)

instance Monad Event where
 (>>=) ev1 f   = ev1 >=> f
 (>>)  ev1 ev2 = ev1 >=> \ _ -> ev2
 return v      = constEvent 0 v

instance MonadZero Event where
 zero  = zeroEvent

instance MonadPlus Event where
 (++)  = (.|.)

{-
 ev +==> f == ev +>>= \ t v -> return (f t v)
 ev ==>  f == map ev f
 ev -=>  v == ev >>= \ _ -> return v
-}

filterEv :: (Time -> Event a) -> (a -> Maybe b) -> Time -> Event b
filterEv evg f ts =
 let 
  ev = evg ts
  evv = unsafePerformIO (newVar ev)
 in
 mkEvt ts
    ( \ t ->
      case occ (unsafePerformIO (readVar evv)) t of
        Nothing -> Nothing 
        Just (tVal,x) ->
          case f x of
            Nothing -> unsafePerformIO (
                            writeVar evv (evg tVal) >>
                            return Nothing)
            Just b  -> Just (tVal, b))


suchThat :: (Time -> Event a) -> (a -> Bool) -> Time -> Event a
suchThat evg pred ts =
 mkEvt ts
  (\ t ->
    case occ (evg t) t of 
     Nothing -> Nothing
     Just (tVal,x) ->
       if pred x then
          Just (tVal,x)
       else
          Nothing)

timeIs :: Time -> Event ()

-- Like predicate (time ==* tVal) (-infinity)
timeIs tVal = constEvent tVal ()

constEvent :: Time -> a -> Event a
constEvent t v = 
  unsafePerformIO (
    newVar (Right occ) >>= \ c ->
    return (Event c))
  where occ = (t, v)

zeroEvent :: Event a
zeroEvent =
 unsafePerformIO (
   newVar (Left (0,(\ _ -> Nothing))) >>= \ c ->
   return (Event c))


{- Not yet re-implemented:

infixl 9 `snapshot`
snapshot :: Event a -> Behavior b -> Event (a,b)
snapshot  ev b = ev +=> \ t a -> (a, b `at` t)

predicate :: Behavior Bool -> Time -> Event ()

-}


-- Test cases

tstE :: Event a -> [EventOcc a]

tstE e = take 11 (map (occ e) [tstEStart ..])

tstEStart = 0::Time

-- e1 = predicate (time*time ==* 3) tstEStart
-- e2 = predicate (time + 1  ==* 2) tstEStart

e1 = timeIs (sqrt 3)
e2 = timeIs 1


e3,e4 :: Event Time
e3 = e1 +=> (\t x -> t)

e4 = e3 ==> (^2)

e5 = e1 .|. e2

e6 = e2 .|. e1

e7 = timeIs 5
