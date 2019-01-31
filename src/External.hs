-- External events.
--
-- Last modified Tue Oct 15 12:42:49 1996
--
-- This stuff needs overhauling.  External events don't sensibly
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

type EventVar a = MutVar (EventOcc a)
 {-
   newStateEvent :: Time -> IO (EventVar a, Event a)
   stateEvent    :: EventVar a -> Event a
 -}


newStateEvent :: Time -> IO (EventVar a, Event a)
newStateEvent t0 =  
 newVar Nothing >>= \ var ->
 return (var, stateEvent t0 var)

-- The event of an EventVar being set to something

stateEvent :: Time -> EventVar a -> Event a

{-
stateEvent t0 var =
  Event (Behavior (map varFun))
  where
    varFun t =
      unsafePerformIO $
      readVar var >>= \ mbOcc ->
      return (mbOcc >>= \ (tVal, _) -> if tVal<t then mbOcc else Nothing)
-}


stateEvent t0 var = Event (Behavior (map varFun))
 where
  varFun t =
    unsafePerformIO (
    readVar var >>= \ mbOcc ->
    case mbOcc of
       -- If already set, compare times.
       Just (tVal, _)  ->
          -- putStrLn ("tVal == " ++ show tVal ++ ", t == " ++ show t) >>
          return (if tVal<t then mbOcc else Nothing)
       Nothing   -> 
         -- If not set yet, say that tVal<t.  BUG: the state could
         -- later get changed to hold a tVal>=t.
 --        putStrLn ("No hit: "++show t) >>
	 return Nothing )

-- External event generator state.  For things like mouse button press or
-- window close.

type EGVar a = MutVar (EventVar a, Event a)

newExternalEGen :: Time -> IO (EGVar a, Time -> Event a)

newExternalEGen t0 =
  -- Make new EventVar/Event pair and initialize a new EGVar with it.
  newStateEvent t0        >>= \ eventVarAndEvent ->
  newVar eventVarAndEvent >>= \ egVar ->
  return (egVar, stateEventGen egVar)

  where
    stateEventGen egVar t0 =
      -- Just return the current Event.  BUG: the contents of egVar
      -- could get/have gotten changed between t0 and now.  Should do some
      -- buffering?
      unsafePerformIO (
        readVar egVar >>= \ (_, ev) ->
--      putStrLn ("stateEvent: " ++ show t0) >>
        return ev)

-- Cause an internalized event to fire.

fireExternalEGen :: Text a => String -> EGVar a -> Time -> a -> IO ()

fireExternalEGen eventName egVar occTime occValue =
  -- Set the current EventVar to have occurred.
  -- putStrLn ("Firing " ++ eventName ++ " " ++ show occValue ++ " at time " ++ show occTime) >>
  readVar egVar    >>=  \ (eventVar, _) ->
  writeVar eventVar (Just (occTime, occValue)) >>
    
  -- Replace it with a new EventVar/Event pair.
  newStateEvent occTime >>= writeVar egVar

-- Implementation note: these newStateEvent's created upon external event
-- occurrence will often not get used, in which case the stateEvent won't
-- really get created, thanks to laziness.  (Right?)

