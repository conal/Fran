-- External events.
--
-- Last modified Thu Sep 12 14:07:03 1996

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

stateEvent t0 var =
  Event (Behavior (map varFun))
  where
    varFun t =
      unsafePerformIO $
      readVar var >>= \ mbOcc ->
      return (mbOcc >>= \ (tVal, _) -> if tVal<t then mbOcc else Nothing)


{-
stateEvent t0 var =
  Event (Behavior (map . varFun))
  where
    varFun t =
      unsafePerformIO (
      readVar var >>= \ mbOcc ->
      case mbOcc of
	-- If already set, compare times.
	Just (tVal, _)  -> return (if tVal<t then mbOcc else Nothing)
      -- If not set yet, say that tVal<t.  BUG: the state could
      -- later get changed to hold a tVal>=t.
      | Nothing   -> 
--        putStrLn ("No hit: "++show t) >>
          return Nothing)
-}

-- External event generator state.  For things like mouse button press or
-- window close.

type EGVar a = MutVar (EventVar a, Event a)

newExternalEGen :: Time -> IO (EGVar a, Time -> Event a)

newExternalEGen t0 =
  -- Make new EventVar/Event pair and initialize a new EGVar with it.
  newStateEvent t0        >>= \ eventVarAndEvent ->
  newVar eventVarAndEvent >>= \ egVar ->
  return (egVar, stateEvent egVar)

  where
    stateEvent egVar t0 =
      -- Just return the current Event.  BUG: the contents of egVar
      -- could get/have gotten changed between t0 and now.  Should do some
      -- buffering?
      unsafePerformIO (
        readVar egVar >>= \ (_, ev) ->
--      putStrLn ("stateEvent: " ++ show t0) >>
        return ev)

-- Cause an internalized event to fire.

fireExternalEGen :: Time -> a -> EGVar a -> IO ()

fireExternalEGen occTime occValue egVar =
  -- Set the current EventVar to have occurred.
  readVar egVar    >>=  \ (eventVar, _) ->
  writeVar eventVar (Just (occTime, occValue)) >>
    
  -- Replace it with a new EventVar/Event pair.
  newStateEvent occTime >>= writeVar egVar

-- Implementation note: these newStateEvent's created upon external event
-- occurrence will often not get used, in which case the stateEvent won't
-- really get created, thanks to laziness.  (Right?)

