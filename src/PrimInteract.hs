-- (single) user interaction primitives.
--
-- These functions should not be used directly.  See Interaction.hs.
--
-- Last modified Tue Oct 15 12:27:51 1996

module PrimInteract 
        (
         initUser,        -- :: IO ()
         primBP,          -- :: Time -> Event (Point2,Bool,Bool)
         primLBP,primLBR, -- :: Time -> Event Point2
         primRBP,primRBR, -- :: Time -> Event Point2

         primKP,          -- :: Time -> Event Char
         primKR,          -- :: Time -> Event Char
         primMousePos,    -- :: Time -> Event Point2
         primViewSz,      -- :: Time -> Event Vector2
         primFPS,         -- :: Time -> Event Double

         -- For ShowImage only (ToDo: create an InteractImpl interface)
         firePrimBPEv,
         firePrimKeyEv,
         firePrimMouseEv,
         firePrimViewSize,
         firePrimFPS

        ) where

import Prelude hiding (MutVar,newVar,readVar,writeVar)
import MutVar
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



-- Initialise the interaction `devices'

initUser :: IO ()
initUser =
 readVar primBPEGVar     >>
 readVar primKeyEGVar    >>
 readVar primMouseEGVar  >>
 readVar primViewSzEGVar >>
 readVar primFPSEGVar    >>
 return ()

primBPEGVar = 
 unsafePerformIO (
   newExternalEGen 0        >>= \ (egv, tev) ->
   writeVar primBPTEv tev   >>
   return egv)

-- Shortcut used by toplevel ev.loop

firePrimBPEv :: Time -> Point2 -> Bool -> Bool -> IO ()
firePrimBPEv t pt left press = 
-- putStrLn ("firing: "++show t) >>
 fireExternalEGen "button" primBPEGVar t (pt,left,press)

primBPTEv :: MutVar (Time -> Event (Point2,Bool,Bool))
primBPTEv = unsafePerformIO (newVar (error "primBPTEv"))

primBP :: Time -> Event (Point2,Bool,Bool)
primBP = unsafePerformIO (readVar primBPTEv)

filteredBP :: Bool -> Bool -> Time -> Event Point2
filteredBP isLeft isPress = filterEv primBP f
 where
  f (p,left,press) =
    if left==isLeft && press==isPress then
       Just p
    else
       Nothing


-- Derived mouse button functions

primLBP, primLBR, primRBP, primRBR :: Time -> Event Point2

primLBP = filteredBP True  True
primLBR = filteredBP True  False
primRBP = filteredBP False True
primRBR = filteredBP False False


-- Keyboard handling

primKeyEGVar = 
 unsafePerformIO (
   newExternalEGen 0        >>= \ (egv, tev) ->
   writeVar primKeyTEv tev  >>
   return egv)

primKeyTEv :: MutVar (Time -> Event (Char,Bool))
primKeyTEv = unsafePerformIO (newVar (error "primKeyTEv"))

firePrimKeyEv :: Time -> Char -> Bool -> IO ()
firePrimKeyEv t ch press = 
 fireExternalEGen "key" primKeyEGVar t (ch,press)

primKey :: Time -> Event (Char,Bool)
primKey = unsafePerformIO (readVar primKeyTEv)

filteredKey :: Bool -> Time -> Event Char

filteredKey isPress = filterEv primKey f
 where
  f (ch,press) =
    if press==isPress then
       Just ch
    else
       Nothing

primKP, primKR :: Time -> Event Char

primKP = filteredKey True
primKR = filteredKey False



-- Tracking mouse positions

primMouseEGVar = 
 unsafePerformIO (
   newExternalEGen 0          >>= \ (egv, tev) ->
   writeVar primMouseTEv tev  >>
   return egv)

primMouseTEv :: MutVar (Time -> Event Point2)
primMouseTEv = unsafePerformIO (newVar (error "primMouseTEv"))

firePrimMouseEv :: Time -> Point2 -> IO ()
firePrimMouseEv = fireExternalEGen "mouse" primMouseEGVar

primMousePos :: Time -> Event Point2
primMousePos = unsafePerformIO (readVar primMouseTEv)


primViewSzEGVar = 
 unsafePerformIO (
   newExternalEGen 0            >>= \ (egv, tev) ->
   writeVar primViewSzTEv tev   >>
   return egv)

{- Shortcut used by toplevel ev.loop -}
firePrimViewSize :: Time -> Vector2 -> IO ()
firePrimViewSize t v  =
  -- trace ("view size " ++ show v ++ " at time " ++ show t ++ "\n") $
  fireExternalEGen "view size" primViewSzEGVar t v

primViewSzTEv :: MutVar (Time -> Event Vector2)
primViewSzTEv = unsafePerformIO (newVar (error "primViewSzTEv"))

primViewSz :: Time -> Event Vector2
primViewSz = unsafePerformIO (readVar primViewSzTEv)

primFPSEGVar = 
 unsafePerformIO (
   newExternalEGen 0         >>= \ (egv, tev) ->
   writeVar primFPSTEv tev   >>
   return egv)

{- Shortcut used by toplevel ev.loop -}
firePrimFPS :: Time -> Double -> IO ()
firePrimFPS = fireExternalEGen "fps" primFPSEGVar

primFPSTEv :: MutVar (Time -> Event Double)
primFPSTEv = unsafePerformIO (newVar (error "primFPSTEv"))

primFPS :: Time -> Event Double
primFPS = unsafePerformIO (readVar primFPSTEv)

