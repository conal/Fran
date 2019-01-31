-- External events.
--
-- Last modified Wed Oct 23 12:15:33 1996
--
-- This stuff needs an overhaul.  External events don't sensibly
-- time-transform, and if one asks for the first occurrence after a time
-- in the past or future, the wrong thing happens (silently).

module External where

import Prelude hiding (MutVar,newVar,readVar,writeVar)
import MutVar

import Behavior
import Event
import Win32 (MilliSeconds)

-- A piece of state that can hold an event time and value.  Initialized to
-- Nothing and set to something at most once.
-- SOF note:
--    Hmm, an application for IVars? (Concurrent Haskell used (still has?)
--    support for IVars, modelling I-structures.)
--   

type EventVar a = MutVar (Maybe (Time, a))
 {-
   newStateEvent :: Time -> IO (EventVar a, Event a)
   stateEvent    :: EventVar a -> Event a
 -}


newStateEvent :: String -> Time -> IO (EventVar a, Event a)

newStateEvent eventName t0 =  
 newVar Nothing >>= \ var ->
 return (var, stateEvent eventName t0 var)

-- The event of an EventVar being set to something

stateEvent :: String -> Time -> EventVar a -> Event a

stateEvent eventName t0 var = event
 where
  event = Event sample
  sample t =
    unsafePerformIO (
    readVar var >>= \ mbOcc ->
    return $
    case mbOcc of
       -- If already set, compare times.
       Just (te, x)  ->
          --putStrLn (eventName ++ ": te == " ++ show te ++ ", t == " ++ show t) >>
          if te<t then  Occ te x  else  NonOcc event
       Nothing   -> 
         -- If not set yet, say that te >= t.  BUG: the state could
         -- later get changed to hold a te < t.
         --putStrLn (eventName ++ ": No hit at " ++ show t) >>
	 NonOcc event )

-- External event generator state.  For things like mouse button press or
-- window close.

type EGVar a = MutVar (EventVar a, Event a)

newExternalEGen :: String -> Time -> IO (EGVar a, Time -> Event a)

newExternalEGen eventName t0 =
  -- Make new EventVar/Event pair and initialize a new EGVar with it.
  --putStrLn ("newExternalEGen " ++ eventName ++ " " ++ show t0) >>
  newStateEvent eventName t0        >>= \ eventVarAndEvent ->
  newVar eventVarAndEvent >>= \ egVar ->
  let
    stateEventGen tStart =
      -- Just return the current Event.  BUG: the contents of egVar
      -- could get/have gotten changed between t0 and now.  Should do some
      -- buffering?
      unsafePerformIO (
        readVar egVar >>= \ (_, ev) ->
        --putStrLn ("stateEventGen (" ++ eventName ++ "): " ++ show tStart) >>
        return ev)
  in
  return (egVar, stateEventGen)


-- Cause an internalized event to fire.

fireExternalEGen :: Text a => String -> EGVar a -> Time -> a -> IO ()

fireExternalEGen eventName egVar occTime occValue =
  -- Set the current EventVar to have occurred.
  --putStrLn ("Firing " ++ eventName ++ " " ++ show occValue ++ " at time " ++ show occTime) >>
  readVar egVar    >>=  \ (eventVar, _) ->
  writeVar eventVar (Just (occTime, occValue)) >>
    
  -- Replace it with a new EventVar/Event pair.
  newStateEvent eventName occTime >>= writeVar egVar

-- Implementation note: these newStateEvent's created upon external event
-- occurrence will often not get used, in which case the stateEvent won't
-- really get created, thanks to laziness.

