-- External events.
--
-- Last modified Wed Nov 06 10:33:44 1996
--
-- This stuff needs an overhaul.  External events don't sensibly
-- time-transform, and if one asks for the first occurrence after a time
-- in the past or future, the wrong thing happens (silently).

module External(
	module External
	) where

import IORef(Ref,newRef,getRef,setRef)
import IOExtensions(unsafePerformIO)

import Behavior
import Event
import Win32 (MilliSeconds)
import Monad (when)

-- A piece of state that can hold an event time and value.  Initialized to
-- Nothing and set to something at most once.
-- SOF note:
--    Hmm, an application for IVars? (Concurrent Haskell used (still has?)
--    support for IVars, modelling I-structures.)
--   

type EventVar a = Ref (Maybe (Time, a))
 {-
   newStateEvent :: Time -> IO (EventVar a, Event a)
   stateEvent    :: EventVar a -> Event a
 -}


newStateEvent :: String -> Time -> IO (EventVar a, Event a)

newStateEvent eventName t0 =  
 newRef Nothing >>= \ var ->
 return (var, stateEvent eventName t0 var)

-- The event of an EventVar being set to something

stateEvent :: String -> Time -> EventVar a -> Event a

stateEvent eventName t0 var = event
 where
  event = Event sample
  sample t =
    unsafePerformIO (
    getRef var >>= \ mbOcc ->
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

type EGVar a = Ref (EventVar a, Event a)

initExternalEGen :: String -> EGVar a -> Time -> IO ()
initExternalEGen eventName egVar t0 =
  newStateEvent eventName t0 >>= \ eventVarAndEvent ->
  setRef egVar eventVarAndEvent

newExternalEGen :: String -> IO (EGVar a, Time -> Event a)

newExternalEGen eventName =
  -- Make new EventVar/Event pair and initialize a new EGVar with it.
  -- when (eventName == "button") (putStrLn $ "newExternalEGen " ++ eventName) >>
  newRef undefined >>= \ egVar ->
  let
    stateEventGen tStart =
      -- Just return the current Event.  BUG: the contents of egVar
      -- could get/have gotten changed between t0 and now.  Should do some
      -- buffering?
      unsafePerformIO (
        getRef egVar >>= \ (_, ev) ->
        --putStrLn ("stateEventGen (" ++ eventName ++ "): " ++ show tStart) >>
        return ev)
  in
  return (egVar, stateEventGen)


-- Cause an internalized event to fire.

fireExternalEGen :: Show a => String -> EGVar a -> Time -> a -> IO ()

fireExternalEGen eventName egVar occTime occValue =
  -- Set the current EventVar to have occurred.
  --when (eventName == "button") (putStrLn $ "Firing " ++ eventName ++ " " ++ show occValue ++ " at time " ++ show occTime) >>
  getRef egVar    >>=  \ (eventVar, _) ->
  setRef eventVar (Just (occTime, occValue)) >>
    
  -- Replace it with a new EventVar/Event pair.
  newStateEvent eventName occTime >>= setRef egVar

-- Implementation note: these newStateEvent's created upon external event
-- occurrence will often not get used, in which case the stateEvent won't
-- really get created, thanks to laziness.

