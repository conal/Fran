-- The "event algebra".
-- 
-- Last modified Thu Jul 17 16:50:40 1997
--
-- To do:
--
--  + Users and interaction (easy)
--  + Some safe and simple caching (lazy evaluation for occurrences)


module Event where

import BaseTypes

import Maybe (isJust)
import Trace

infixr 1 `untilB`

infixl 3 ==>
infixl 3 -=>
infixl 2 .|.

infixl 9 `filterE`, `suchThat`

--infixl 1 +>>=   -- generalization of >>=


-- Represent an event as a sequence of possible occurrences.  The
-- non-occurrences are kept in order to determine lower bounds on
-- occurrence time.

newtype Event a = Event [PossOcc a]

type PossOcc a = (Time, Maybe a)


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
-- meant for helping to define ==>, -=>, withNextE, and withTimeE

handleE :: Event a -> (Time -> a -> Event a -> b) -> Event b

Event possOccs `handleE` f = Event (loop possOccs)
 where
   loop [] = []
   loop (p@(te, mb) : possOccs') =
     (te, map (\x -> f te x (Event possOccs')) mb) : loop possOccs'


{-
Event possOccs ==> f = Event (map g possOccs)
 where
   -- Just massage the data for real occurrences.
   g (te, mb) = (te, map f mb)
-}

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


{- Out for now.  Sorry!  See next comment.

Think about this carefully and do it right.  Needs some kind of
backtracking, since we have to enumarate all occurrences of ev, which are
events themselves, and then all occurrences of them, and present them in
time-sorted order.

-- joinEvent e is the event that occurs when e' occurs, where e' is the
-- event data part of e.  (Should be time adjusted to be no earlier than
-- e.)  This is "join" for the Event monad.  I use it to define +>>=
-- below, which is used to define >>=.  Look into turning this around, so
-- that I can just use "join", as defined in StdLib/Monad.hs.

joinEvent :: Event (Event a) -> Event a

-}


-- Generalization of suchThat, replacing a condition by a maybe value.

filterE :: Event a -> (a -> Maybe b) -> Event b

Event possOccs `filterE` f = Event (map filt possOccs)
 where
   filt (te, Nothing) = (te, Nothing)

   filt (te, Just x)  = (te, case f x of
                               Nothing  -> Nothing
                               Just y   -> Just y)
  


------- Non-primitives

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

-- An event that never happens

neverE :: Event a

neverE = possOccsE []

-- Move time and next event to data.

withNextE :: Event a -> Event (a, Event a)

withNextE e = e `handleE` \ te x e' -> (x,e')

withTimeE :: Event a -> Event (a, Time)

withTimeE e = e `handleE` \ te x e' -> (x,te)


-- Event handler simplifications.  To do: be systemmatic with names and
-- functionality.

ev ==> f  =  ev `handleE` \ te x e' -> f x
ev -=> x  =  ev ==> const x



-- "e +>>= f" is the event e' = f te x, where te and x are the time and
-- value from e.  Note: if e' occurs earlier than e, it will still not be
-- detected until after e.  Thus occurrence time should probably be
-- tweaked to be at least e's.

-- (+>>=) :: Event a -> (Time -> a -> Event b) -> Event b

-- Use +=> to make an event-valued event, and joinEvent to flatten.

-- Removed for now

-- ev +>>= f  =  joinEvent (ev +=> f)

 
-- Filter out event occurrences whose data doesn't satisfy a condition.

suchThat :: Event a -> (a -> Bool) -> Event a

suchThat ev pred =
  filterE ev (\a -> if pred a then Just a else Nothing) 


{-

-- Type class instances.  Monadic stuff.

instance Functor Event where
  map = flip (==>)

instance Monad Event where
 ev >>= f   =  joinEvent (ev ==> f)
 return v   =  constE minPossibleTime v neverEvent -- ???

instance MonadZero Event where
 zero  = neverEvent

instance MonadPlus Event where
 (++)  = (.|.)

-}

-- "Generalized behaviors".

class GBehavior bv where
  untilB    :: bv -> Event bv -> bv
  startTime :: bv -> Time
  afterTime :: bv -> Time -> bv

instance GBehavior (Event a) where
  Event possOccs1 `untilB` Event possOccs2 = Event (loop possOccs1 possOccs2)
   where
     -- No more in first event.  Just use what e2 gives
     loop [] possOccs2  =  --trace "Event untilB: no more LHS occurrences\n" $
                           possOccsOf (joinEOne (Event possOccs2))

     loop pos1@(po1@(te1,mb1) : pos1')
          pos2@(po2@(te2,mb2) : pos2') =
       if te1 <= te2 then
         trace (show te1 ++ " <= " ++ show te2 ++ "\n")$
         po1 : loop pos1' pos2
       else trace (show te1 ++ " > " ++ show te2 ++ " and ")$
            case mb2 of
              Just e2' ->  trace "occurrence\n"$
                           possOccsOf e2'
              Nothing  ->  trace "non-occurrence\n"$
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


-- One choice for the monadic join for events.  This one uses only the
-- first occurrence of the given event.  Another could use them all and
-- interleave.

joinEOne :: Event (Event a) -> Event a

joinEOne (Event possOccs) = Event (loop possOccs)
 where
   loop [] = []

   loop po@((te, mb) : possOccs') =
     (te,Nothing) : 
     case mb of
       Nothing  ->  loop possOccs'
       Just (Event morePossOccs)  ->  morePossOccs

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

-- Like scanr for lists.

scanlE :: (a -> b -> a) -> a -> Event b -> Event a

scanlE f x0 (Event possOccs) = Event (loop x0 possOccs)
 where
  loop _ [] = []

  loop x0 ((te, Nothing) : possOccs') =
    (te, Nothing) : loop x0 possOccs'

  loop x0 ((te, Just x) : possOccs') =
    (te, Just x1) : loop x1 possOccs'
   where
     x1 = f x0 x


withPrevE :: a -> Event a -> Event (a,a)

withPrevE a0 (Event possOccs) = Event (loop a0 possOccs)
 where
   loop aPrev ((te, Nothing) : possOccs') =
     (te, Nothing) : loop aPrev possOccs'

   loop aPrev ((te, Just aNew ) : possOccs') =
     (te, Just (aPrev, aNew)) : loop aNew possOccs'


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
   loop (o@(_,mb) : possOccs') = maybeTrace o : loop possOccs'
    where 
     maybeTrace | not (isJust mb) && flag == TraceOccsE = id
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
