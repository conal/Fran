-- This version is right before I switched to memoized functions.
-- 
-- The "event algebra".


module Event where

import BaseTypes

import Concurrent                           -- for EventChannel
-- import IOExtensions (unsafeInterleaveIO) -- for getChanContents

import Maybe (isJust)
import Trace

infixr 1 `untilBE`

infixl 3 ==>
infixl 3 -=>
infixl 2 .|.

infixl 3 `handleE`, `filterE`
       , `withElemE`, `withElemE_`
       , `withPrevE`, `withPrevE_`
       , `suchThat`, `suchThat_`


-- Represent an event as a sequence of possible occurrences.  The
-- non-occurrences are kept in order to determine lower bounds on
-- occurrence time.

newtype Event a = Event [PossOcc a]
type  PossOcc a = (Time, Maybe a)


instance Show (Event a) where showsPrec _ _ = showString "<<event>>"

-- The abstract interface.

occs :: Event a -> [Time] -> [Maybe (Time, a)]

Event possOccs `occs` ts = loop possOccs ts
 where
   -- Out of occurrences.
   loop [] ts = --trace "Event/occ: no more possOccs\n" $
                -- Or (map (const Nothing) ts), to get a list of the same
                -- length.
                repeat Nothing

   -- The first occurrence is *at least* at time te.
   loop possOccs@((te, mb) : possOccs') ts@(t:ts')
     -- Sample time is before first possible occurrence.  Report one
     -- non-occurrence and look for more.
     | t <= te  =  --trace ("Event/occ: t <= " ++ show te ++ "\n") $
                   Nothing : loop possOccs ts'

     | otherwise = --trace ("Event/occ: t > " ++ show te ++ "\n") $
        case mb of
          -- A non-occurrence at te, so look for more.
          Nothing -> --trace ("non-occurrence at " ++ show te ++ "\n") $
                     loop possOccs' ts
          -- A genuine occurrence strictly before t, so report it along
          -- with the remainder.
          Just x  -> --trace ("occurrence at " ++ show te ++ "\n") $
                     Just (te,x) : loop possOccs' ts'
   loop _ _ = error "no time stream"	-- GSL

afterTimesE :: Event a -> [Time] -> [Event a]

Event possOccs `afterTimesE` ts =
 --trace ("Starting afterTimesE\n") $
 loop possOccs ts
 where
   -- Out of occurrences.  Equivalent to neverE hereafter.
   loop [] ts = --trace "  Event/afterTimesE: no more possOccs\n" $
                -- Or (map (const neverE) ts), to get a list of the same
                -- length.
                repeat neverE

   -- The first occurrence is *at least* at time te.
   loop possOccs@((te, mb) : possOccs') ts@(t:ts')
     -- Sample time is before first possible occurrence.  Report one
     -- non-occurrence and look for more.
     | te <= t   = --trace ("  Event/afterTimesE: " ++ show te ++ " <= " ++ show t ++ "\n") $
                   loop possOccs' ts

     | otherwise = --trace ("  Event/afterTimesE: " ++ show te ++ " >  " ++ show t ++ "\n") $
                   Event possOccs : loop possOccs ts'
   loop _ _ = error "no time stream"	-- GSL


-- An event from a list of possible occurrences
possOccsE :: [PossOcc a] -> Event a
possOccsE = Event

-- Event from a list of definite occurrences
occsE :: [(Time,a)] -> Event a
occsE pairs = possOccsE (map (\ (t,x) -> (t, Just x)) pairs)

-- Extract the possible occurrences.  Don't export, since it violates the
-- event abstraction, i.e., distinguishes between semantically equal
-- events.
possOccsOf ::Event a -> [PossOcc a]
possOccsOf (Event possOccs) = possOccs


-- The event "ev `handleE` f" occurs exactly when ev occurs.  Its value is
-- (f te x nextEv), where te and x are the time and value of ev's
-- occurrence, and nextEv is a remainder event.  This combinator is only
-- meant for helping to define ==>, -=>, withRestE, and withTimeE

handleE :: Event a -> (Time -> a -> Event a -> b) -> Event b
Event possOccs `handleE` f = Event (loop possOccs)
 where
   loop [] = []
   loop ((te, mb) : possOccs') =
     (te, map (\x -> f te x (Event possOccs')) mb) : loop possOccs'


-- The event e .|. e' corresponds to the union of occurrences of e and e',
-- listing e occurrences before e' occurrences when simultaneous.

(.|.) :: Event a -> Event a -> Event a
Event possOccs .|. Event possOccs' = Event (merge possOccs possOccs')
 where
   merge os@(p@(te, mb) : osRest) os'@(p'@(te', mb') : osRest')
     | te  <=  te' =  p  : merge osRest  os'
     | otherwise   =  p' : merge os      osRest'
   merge [] os' = os'
   merge os []  = os

-- Remaps event data and removes some occurrences.  Maybe this guy should be
-- replaced by a simplified function: Event (Maybe a) -> Event a, to be
-- used in conjunction with ==>.

filterE :: Event a -> (a -> Maybe b) -> Event b
Event possOccs `filterE` f = Event (map filt possOccs)
 where
   filt (te, mb) = (te, mb >>= f)

-- Question: are monadic definitions like that of filt above too obscure?
-- It says that Nothing goes to Nothing, and Just x goes to (f x), which
-- might be a Nothing or a Just.  Similarly, handleE uses ++ on Maybe.


-- Pair up members of a list with occurrences of an event
withElemE :: Event a -> [b] -> Event (a,b)
Event possOccs `withElemE` l = Event (loop possOccs l)
 where
   loop ((te, mb) : possOccs') ~bs@(b:bs') =
     (te, mbPair) : loop possOccs' bsNext
    where
      mbPair = map (`pair` b) mb
      bsNext | isJust mb = bs'
             | otherwise = bs
   loop [] _ = []

withElemE_ :: Event a -> [b] -> Event b
e `withElemE_` l = e `withElemE` l ==> snd

----------- Monadic stuff ----------

instance Functor Event where
  map = flip (==>)

instance Monad Event where
 -- Using joinEOne is one choice, but we may want to backtrack.
 ev >>= f   =  joinEOne (ev ==> f)
 return v   =  constE minTime v

instance MonadZero Event where
 zero  = neverE

instance MonadPlus Event where
 (++)  = (.|.)

-- One choice for the monadic join for events.  This one uses only the
-- first occurrence of the given event.  Alternatively, we could do some
-- kind of backtracking, enumerating all occurrences of ev, which are
-- events themselves, and then all occurrences of them, and presenting the
-- whole batch of them in time-sorted order.

joinEOne :: Event (Event a) -> Event a
joinEOne (Event possOccs) = Event (loop possOccs)
 where
   loop [] = []

   loop po@((te, mb) : possOccs') =
     (te,Nothing) : 
     case mb of
       Nothing                    ->  loop possOccs'
       Just (Event morePossOccs)  ->  morePossOccs

------------- Event as generalized behavior -------------

Event possOccs1 `untilBE` Event possOccs2 = Event (loop possOccs1 possOccs2)
 where
   -- No more in first event.  Just use what e2 gives
   loop pos1@(po1@(te1,mb1) : pos1')
        pos2@(po2@(te2,mb2) : pos2') =
     if te1 <= te2 then
       -- Still old event.  Emit first possible occurrence.
       --trace (show te1 ++ " <= " ++ show te2 ++ "\n")$
       po1 : loop pos1' pos2
     else --trace (show te1 ++ " > " ++ show te2 ++ " and ")$
          case mb2 of
            Just e2' ->  -- Real first occurrence.  Switch.
                         --trace "occurrence\n"$
                         possOccsOf e2'
            Nothing  ->  --trace "non-occurrence\n"$
                         (te2,Nothing) : loop pos1 pos2'
   loop [] possOccs2  =  --trace "Event untilB: no more LHS occurrences\n" $
                         possOccsOf (joinEOne (Event possOccs2))
   loop possOccs1 []  = possOccs1	-- GSL

-- Like scanl for lists.  Warning! Do not use for GBehavior a, since it
-- will not get "aged".  See accumB.  Note: maybe the accumulator should
-- be forced at each point.  ## To do: make "Ageable" superclass of
-- GBehavior, with afterTimes, require it here, and do the aging.  Define
-- a function "staticAfterTimes" for easy definition of static instances
-- of Ageable.

scanlE :: (a -> b -> a) -> a -> Event b -> Event a
scanlE f x0 (Event possOccs) = Event (loop x0 possOccs)
 where
  loop _ [] = []

  -- Be careful to make te available right away without forcing mb.
  loop x0 ((te, mb) : possOccs') = (te, mb') : loop x0' possOccs'
   where
     (mb', x0') = case mb of
                    Nothing -> (Nothing, x0)
                    Just x  -> (Just x1, x1) where x1 = f x0 x


-- Warning: untested. ##
withPrevE :: Event a -> a -> Event (a,a)
e `withPrevE` a0 = scanlE (\(older,old) new -> (old,new))
                          (error "no prev", a0)
                          e
{- -- Old def
withPrevE :: Event a -> a -> Event (a,a)
(Event possOccs) `withPrevE` a0 = Event (loop a0 possOccs)
 where
   loop aPrev ((te, mb) : possOccs') = (te, mb') : loop aPrev' possOccs'
    where
      (mb', aPrev') = case mb of
                        Nothing   -> (Nothing, aPrev)
                        Just aNew -> (Just (aPrev, aNew), aNew)
-}

withPrevE_ :: Event a -> a -> Event a
e `withPrevE_` a0 = e `withPrevE` a0 ==> snd

-------------- Channel-based (external) events ---------------

-- External event occurrences come in on a channel, together with
-- non-occurrence buffering.
type EventChannel a = Chan (PossOcc a)

-- Make a new channel event, with an initial no-op.  This no-op lets the
-- user be queried for time t0 even when there isn't yet a thread doing
-- more putChan's.

newChannelEvent :: Time -> IO (Event a, EventChannel a)
newChannelEvent t0 =
  do ch <- newChan
     -- The following entry is in case the event gets queried at time t0.
     putChan ch (t0, Nothing)
     contents <- getChanContents ch
     return (possOccsE contents, ch)

-- From the Concurrent Haskell Channel, courtesy of Sigbjorn
-- Should really move into Hugs/lib/hugs/Channel.hs
-- getChanContents :: Show a => Channel a -> IO [a]
-- getChanContents ch =
--  unsafeInterleaveIO (
--  do -- putStrLn "Doing getChan"
--     x  <- getChan ch
--     --print x
--     xs <- unsafeInterleaveIO (getChanContents ch)
--     return  (x:xs) )

-------------- non-primitives --------------

-- List version of .|.

anyE :: [Event a] -> Event a
anyE = foldr (.|.) neverE

timeIs :: Time -> Event ()
timeIs te  =  possOccsE [(te, Just ())]

constE :: Time -> a -> Event a
constE te x = timeIs te -=> x

-- An alarm clock going off at a regular interval.
alarmE :: Time -> Time -> Event ()
alarmE t0 dt = occsE (map (`pair` ()) [t0+dt, t0+2*dt ..])

-- The event that never happens.  Identity for .|.
neverE :: Event a
neverE = possOccsE []

-- Move time and remainder event into data.
withRestE :: Event a -> Event (a, Event a)
withRestE e = e `handleE` \ te x e' -> (x,e')

withTimeE :: Event a -> Event (a, Time)
withTimeE e = e `handleE` \ te x e' -> (x,te)

-- Just expose the remainder event.  Could be called withRestE_.
nextE :: Event a -> Event (Event a)
nextE e = e `handleE` \ te x e' -> e'

withRestE_ :: Event a -> Event (Event a)
withRestE_ = nextE


-- Event handler simplifications.  ## Maybe rename "-=>" to "==>-", and in
-- general use a trailing "-" in operator names just as "_" in alphabetic
-- names.
ev ==> f  =  ev `handleE` \ te x e' -> f x
ev -=> x  =  ev ==> const x


-- Filter out event occurrences whose data doesn't satisfy a condition.

suchThat :: Event a -> (a -> Bool) -> Event a
ev `suchThat` pred =
  filterE ev (\a -> if pred a then Just a else Nothing) 

suchThat_ :: Event a -> (a -> Bool) -> Event ()
ev `suchThat_` pred = ev `suchThat` pred -=> ()

-------------- Debugging support -------------

{-
-- Identity, but checks that an event has strictly increasing times.
-- Currently (Sep 29 1997), disp in Spritify.hs does no make a valid user
-- event.

validateE :: Event a -> Event a

validateE (Event possOccs) = Event (loop possOccs minTime)
 where
   loop [] _ = []
   loop (o@(te, _) : possOccs') prevT =
     if (prevT > te) then
       error $ "validateE failed with " ++ show (prevT,te)
     else
       o : loop possOccs' te

-}

-- Event tracing support

data TraceEFlag = TraceOccsE | TraceAllE   deriving (Show, Eq)

traceE :: Show a => String -> TraceEFlag -> Event a -> Event a
traceE prefix flag (Event possOccs) = Event (loop possOccs)
 where
   loop [] = []
   loop (o@(te,mb) : possOccs') = (te, maybeTrace mb) : loop possOccs'
    where 
     maybeTrace
      | not (isJust mb) && flag == TraceOccsE = id
      | otherwise                             =
           trace ("traceE " ++ prefix ++ ": " ++ show o ++ "\n")



-------- Testing

tstE f = take 25 l  where  Event l = {-validateE-} (f 0)                 

e0 t0 = (neverE :: Event ())

e1 t0 = timeIs (2+t0)

e2 t0 = alarmE t0 0.1

-- Alarm with deltaT as data
e3 t0 = withTimeE (e2 t0) ==> subtract t0 . snd

-- On a half or third boundary.
e4 t0 = alarmE t0 (1/2) -=> "left" .|. alarmE t0 (1/3) -=> "right"

-- Go off every 0.1 second for a while and then every 0.2 second
e5 t0 = alarmE t0 0.1 `untilBE` timeIs (2+t0) -=> alarmE (2+t0) 0.2
