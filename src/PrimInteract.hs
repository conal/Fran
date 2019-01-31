-- (single) user interaction primitives.
--
-- These functions should not be used directly.  See Interaction.hs.
--
-- Last modified Mon Oct 21 11:36:18 1996

module PrimInteract 
        (PrimEV(..)      
	,button         -- :: PrimEV (Point2,Bool,Bool)
	,key	        -- :: PrimEV Point2
	,mousePos       -- :: PrimEV Point2
	,viewSz	        -- :: PrimEV Vector2
	,fps	        -- :: PrimEV Double
	,initUser       -- :: IO ()
		      
        ,lbp,lbr     	-- :: Time -> Event Point2
        ,rbp,rbr     	-- :: Time -> Event Point2
		      
        ,keyPress       -- :: Time -> Event Char
        ,keyRelease     -- :: Time -> Event Char
        ) where

import IORef(Ref,newRef,getRef,setRef)
import IOExtensions(unsafePerformIO)

import External
import Behavior
import Event
import Point2
import Vector2

{-
 Adding interaction - attempt 1:
  (basic support - does not address picking)

  From within a purely functional setting, an event like
  LBP reaches out and catches mbutton Event wrt. some start time. 
  To be able to do this without passing in an environment to the
  behaviours and/or lift them into IO, we use unsafePerformIO
  to reach out and grab the current state of the mouse.

  Problem/Bug:
        Two events (LBP t0) (LBP t0) do not share underlying
        state, so 
-}

----------------------------------------------------------------
-- Primitive Event Dowidgets
--
-- NB These contain global mutable variables and are therefore
-- not referentially transparent.  Care should be taken not to
-- duplicate these variables or "substitute equals for equals".
---------------------------------------------------------------

button   :: PrimEV (Point2,Bool,Bool)
key      :: PrimEV (Char, Bool)
mousePos :: PrimEV Point2
viewSz   :: PrimEV Vector2
fps      :: PrimEV Double

button   = unsafePerformIO $ mkPrimEV "button"
key      = unsafePerformIO $ mkPrimEV "key"
mousePos = unsafePerformIO $ mkPrimEV "mousePos"
viewSz   = unsafePerformIO $ mkPrimEV "viewSz"
fps      = unsafePerformIO $ mkPrimEV "fps"

-- Initialise the interaction `devices'

initUser :: IO ()
initUser = do
  reset button
  reset key
  reset mousePos
  reset viewSz
  reset fps

----------------------------------------------------------------
-- Mouse button functions
----------------------------------------------------------------

lbp, lbr, rbp, rbr :: Time -> Event Point2
lbp = filteredBP True  True
lbr = filteredBP True  False
rbp = filteredBP False True
rbr = filteredBP False False

filteredBP :: Bool -> Bool -> Time -> Event Point2
filteredBP isLeft isPress = filterEv (getEvent button) f
 where
  f (p,left,press) | left==isLeft && press==isPress = Just p
                   | otherwise                      = Nothing

----------------------------------------------------------------
-- Keyboard handling utilities
----------------------------------------------------------------

keyPress, keyRelease :: Time -> Event Char
keyPress   = filteredKey True
keyRelease = filteredKey False

filteredKey :: Bool -> Time -> Event Char
filteredKey isPress = filterEv (getEvent key) f
 where
  f (ch,press) | press==isPress = Just ch
               | otherwise      = Nothing

----------------------------------------------------------------
-- Package them up in a structure for export
----------------------------------------------------------------

data Show a => PrimEV a = 
  PrimEV {
    reset    :: IO (),
    fire     :: Time -> a -> IO (),
    getEvent :: Time -> Event a
    }

mkPrimEV :: Show a => String -> IO (PrimEV a)
mkPrimEV nm = 
  newExternalEGen nm >>= \ (egv, tev) ->
  return $ PrimEV { 
	     reset    = initExternalEGen nm egv 0,
	     fire     = fireExternalEGen nm egv,
	     getEvent = tev
	     }
