-- The "event algebra".
-- 
-- Last modified Thu Aug 07 10:41:05 1997
--
-- To do:
--
--  + Re-examine the notion of start times for events.  The current
--    implementation is bogus!
--  + Users and interaction (easy)
--  + Some safe and simple caching (lazy evaluation for occurrences)


module Event where

import BaseTypes

import Channel                           -- for EventChannel
import IOExtensions (unsafeInterleaveIO) -- for getChanContents

import Maybe (isJust)
import Trace

infixr 1 `untilB`, `untilF`

infixl 3 ==>
infixl 3 -=>
infixl 2 .|.

infixl 9 `handleE`, `filterE`
       , `withElemE`, `withElemE_`
       , `afterE`, `afterE_`
       , `withPrevE`, `withPrevE_`
       , `suchThat`


-- Represent an event as a sequence of possible occurrences.  The
-- non-occurrences are kept in order to determine lower bounds on
-- occurrence time.

newtype Event a = Event [PossOcc a]
type  PossOcc a = (Time, Maybe a)


instance Show (Event a) where showsPrec _ _ = showString "<<event>>"

-- The abstract interface.  Well, 

occ :: Event a -> Time -> (Maybe (Time, a), Event a)
Event possOccs `occ` t = loop possOccs
 where
   -- Out of occurrences
   loop [] = --trace "Event/occ: no more possOccs\n" $
             (Nothing, neverE)

   -- The first occurrence is *at least* at time te.
   loop po@((te, mb) : possOccs')
     -- Sample time is before first possible occurrence.  Try later.
     | t <=  te  =  --trace "Event/occ: t<te\n" $
                    (Nothing, Event po)
{-
     -- Sample time is right at first occurrence time.  Try later, but
     -- without the first possible occurrence, since it will then be in
     -- the past.
     | t == te  =  --trace "Event/occ: t==te\n" $
                   (Nothing, Event possOccs')
-}
     | otherwise = --trace "Event/occ: t>te, " $
        case mb of
          -- A non-occurrence at te, so look for more
          Nothing -> --trace "non-occurrence\n" $
                     loop possOccs'
          -- A genuine occurrence strictly before t, so report it along
          -- with the remainder.
          Just x  -> --trace "occurrence\n" $
                     (Just (te,x), Event possOccs')

-- An event from a list of possible occurrences

possOccsE :: [PossOcc a] -> Event a
possOccsE = Event

occsE :: [(Time,a)] -> Event a
occsE pairs = possOccsE (map (\ (t,x) -> (t, Just x)) pairs)


-- Don't export
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


-- The event e .|. e' corresponds to the earlier of e and e', prefering
-- e if they occur simultaneously.

(.|.) :: Event a -> Event a -> Event a
Event possOccs .|. Event possOccs' = Event (merge possOccs possOccs')
 where
   merge os@(p@(te, mb) : osRest) os'@(p'@(te', mb') : osRest')
     -- If one possible occurrence is earlier, include it first.
     | te  <  te' =  p  : merge osRest  os'
     | te' <  te  =  p' : merge os      osRest'
     -- If simultaneous, then take the Just if there is one, but prefer
     -- the left event.
     | otherwise  =  (te, mb ++ mb') : merge osRest osRest'

-- Generalization of suchThat, replacing a condition by a maybe value.

filterE :: Event a -> (a -> Maybe b) -> Event b
Event possOccs `filterE` f = Event (map filt possOccs)
 where
   filt (te, mb) = (te, mb >>= f)

-- Question: are monadic definitions like that of filt above too obscure?
-- It says that Nothing goes to Nothing, and Just x goes to (f x), which
-- might be a Nothing or a Just.  Similarly, .|. uses ++ on Maybe.


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

withElemE_ :: Event a -> [b] -> Event b
e `withElemE_` l = (e `withElemE` l) ==> snd

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

class GBehavior bv where
  untilB    :: bv -> Event bv -> bv
  afterTime :: bv -> Time     -> bv
  startTime :: bv             -> Time

-- I thought this would be useful.  However, the event should really be a
-- function of bv, which breaks the mold.
instance (GBehavior bv, GBehavior bv') => GBehavior (bv -> bv') where
 (f `untilB` e) bv = f bv `untilB` e `afterE` bv ==> uncurry ($)
 -- ...

-- Instead, use this guy.  Should there be one version for GBehavior a
-- that does the afterE and one for non-GBehavior a that doesn't??
untilF :: (GBehavior bv, GBehavior a)
       => (a -> bv) -> (a -> Event (a -> bv)) -> (a -> bv)
(f `untilF` e) a = f a `untilB` e a `afterE` a ==> uncurry ($)

instance (GBehavior bv, GBehavior bv') => GBehavior (bv,bv') where
 (bv, bv') `untilB` e    = (bv `untilB` e ==> fst, bv' `untilB` e ==> snd)
 (bv, bv') `afterTime` t = (bv `afterTime` t, bv' `afterTime` t)
 startTime (bv, bv')     = startTime bv `max` startTime bv'


instance GBehavior (Event a) where
  Event possOccs1 `untilB` Event possOccs2 = Event (loop possOccs1 possOccs2)
   where
     -- No more in first event.  Just use what e2 gives
     loop [] possOccs2  =  --trace "Event untilB: no more LHS occurrences\n" $
                           possOccsOf (joinEOne (Event possOccs2))

     loop pos1@(po1@(te1,mb1) : pos1')
          pos2@(po2@(te2,mb2) : pos2') =
       if te1 <= te2 then
         --trace (show te1 ++ " <= " ++ show te2 ++ "\n")$
         po1 : loop pos1' pos2
       else --trace (show te1 ++ " > " ++ show te2 ++ " and ")$
            case mb2 of
              Just e2' ->  --trace "occurrence\n"$
                           possOccsOf e2'
              Nothing  ->  --trace "non-occurrence\n"$
                           (te2,Nothing) : loop pos1 pos2'

  (Event possOccs) `afterTime` t = Event (loop possOccs)
   where
     loop [] = []
     loop po@((te,_) : possOccs')
       | te <= t   =  loop possOccs'    -- too early to matter
       | otherwise =  po                -- all are > t

  -- startTime is problematic.  There is no way of knowing, since we know
  -- only a lower bound for the first occurrence time.
  startTime (Event [])         = minTime
  startTime (Event ((te,_):_)) = te

afterE :: GBehavior bv => Event a -> bv -> Event (a, bv)
Event possOccs `afterE` b = Event (loop possOccs b)
 where
   loop [] _ = []

   -- Pair up the event data, if any, with the behavior sample. 
   loop ((te,mb) : possOccs') b =
     --  Do I need to force b' to WHNF, so keep things moving?
     -- b' `seq`
     (te, map (`pair` b') mb) : loop possOccs' b'
    where
      b' = b `afterTime` te

afterE_ :: GBehavior bv => Event a -> bv -> Event bv
e `afterE_` b  = (e `afterE` b) ==> snd

-- Like scanl for lists.  Warning! Do not use for GBehavior a, since it
-- will not get "aged".  See accumB.  Note: maybe the accumulator should
-- be forced at each point.  As a lucky side-effect, the (Forceable a)
-- context will lock out GBehaviors.

scanlE :: (a -> b -> a) -> a -> Event b -> Event a
scanlE f x0 (Event possOccs) = Event (loop x0 possOccs)
 where
  loop _ [] = []

  -- Be careful not to make te available right away without forcing mb.
  loop x0 ((te, mb) : possOccs') = (te, mb') : loop x0' possOccs'
   where
     (mb', x0') = case mb of
                    Nothing -> (Nothing, x0)
                    Just x  -> (Just x1, x1) where x1 = f x0 x


withPrevE :: Event a -> a -> Event (a,a)
(Event possOccs) `withPrevE` a0 = Event (loop a0 possOccs)
 where
   loop aPrev ((te, mb) : possOccs') = (te, mb') : loop aPrev' possOccs'
    where
      (mb', aPrev') = case mb of
                        Nothing   -> (Nothing, aPrev)
                        Just aNew -> (Just (aPrev, aNew), aNew)

withPrevE_ :: Event a -> a -> Event a
e `withPrevE_` a0 = (e `withPrevE` a0) ==> snd

-------------- Channel-based (external) events ---------------

-- External event occurrences come in on a channel, together with
-- non-occurrence buffering.
type EventChannel a = Channel (PossOcc a)

-- Make a new user, with an initial no-op.  This lets the user be queried
-- for time t0 even when there isn't yet a thread doing more putChan's.

newChannelEvent :: Time -> IO (Event a, EventChannel a)
newChannelEvent t0 =
  do ch <- newChan
     -- The following entry is in case the event gets queried at time t0.
     putChan ch (t0, Nothing)
     contents <- getChanContents ch
     return (possOccsE contents, ch)
 where
   -- From the Concurrent Haskell Channel, courtesy of Sigbjorn
   -- Should really move into Hugs/lib/hugs/Channel.hs
   -- getChanContents :: Show a => Channel a -> IO [a]
   getChanContents ch =
     unsafeInterleaveIO (
     do -- putStrLn "Doing getChan"
        x  <- getChan ch
        --print x
        xs <- unsafeInterleaveIO (getChanContents ch)
        return  (x:xs) )


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
alarmE t0 dt = occsE (map (`pair` ()) [t0, t0+dt ..])

-- The event that never happens.  Identity for .|.
neverE :: Event a
neverE = possOccsE []

-- Move time and remainder event to data.

withRestE :: Event a -> Event (a, Event a)
withRestE e = e `handleE` \ te x e' -> (x,e')

withTimeE :: Event a -> Event (a, Time)
withTimeE e = e `handleE` \ te x e' -> (x,te)


-- Event handler simplifications.  To do: be systemmatic with names and
-- functionality.

ev ==> f  =  ev `handleE` \ te x e' -> f x
ev -=> x  =  ev ==> const x


-- Filter out event occurrences whose data doesn't satisfy a condition.

suchThat :: Event a -> (a -> Bool) -> Event a
suchThat ev pred =
  filterE ev (\a -> if pred a then Just a else Nothing) 


-------------- Debugging support -------------

-- Identity, but checks that an event has strictly increasing times

validateE :: Event a -> Event a

validateE (Event possOccs) = Event (loop possOccs minTime)
 where
   loop [] _ = []
   loop (o@(te, _) : possOccs') prevT =
     if (prevT > te) then
       error $ "validateE failed with " ++ show (prevT,te)
     else
       o : loop possOccs' te

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

tstE f = take 25 l  where  Event l = validateE (f 0)

e0 t0 = (neverE :: Event ())

e1 t0 = timeIs (2+t0)

e2 t0 = alarmE t0 0.1

-- Alarm with deltaT as data
e3 t0 = withTimeE (e2 t0) ==> subtract t0 . snd

-- On a half or third boundary.
e4 t0 = alarmE t0 (1/2) -=> "left" .|. alarmE t0 (1/3) -=> "right"

-- Go off every 0.1 second for a while and then every 0.2 second
e5 t0 = alarmE t0 0.1 `untilB` timeIs (2+t0) -=> alarmE (2+t0) 0.2
