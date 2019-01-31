{- Primitive (single) user interaction - attempt #1 -}

-- Last modified Tue Sep 17 14:48:44 1996
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
firePrimBPEv t pt left down = 
-- putStrLn ("firing: "++show t) >>
 fireExternalEGen t (pt,left,down) primBPEGVar

primBPTEv :: MutVar (Time -> Event (Point2,Bool,Bool))
primBPTEv = unsafePerformIO (newVar (error "primBPTEv"))

primBP :: Time -> Event (Point2,Bool,Bool)
primBP = unsafePerformIO (readVar primBPTEv)

-- Derived mouse button functions

primLBP :: Time -> Event Point2
primLBP = filterEv (primBP) f
 where
  f (p,left,down) =
    if left && down then
       Just p
    else
       Nothing

primLBR :: Time -> Event Point2
primLBR = filterEv (primBP) f
 where
  f (p,left,down) =
    if left && (not down) then
       Just p
    else
       Nothing

primRBP :: Time -> Event Point2
primRBP = filterEv (primBP) f
 where
  f (p,left,down) =
    if (not left) && down then
       Just p
    else
       Nothing

primRBR :: Time -> Event Point2
primRBR = filterEv (primBP) f
 where
  f (p,left,down) =
    if (not left) && (not down) then
       Just p
    else
       Nothing

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
 fireExternalEGen t (ch,press) primKeyEGVar

primKey :: Time -> Event (Char,Bool)
primKey = unsafePerformIO (readVar primKeyTEv)

primKP :: Time -> Event Char
primKP = filterEv (primKey) f
 where
  f (ch,press) =
    if press then
       Just ch
    else
       Nothing

primKR :: Time -> Event Char
primKR = filterEv (primKey) f
 where
  f (ch,press) =
    if not press then
       Just ch
    else
       Nothing


-- Tracking mouse positions

primMouseEGVar = 
 unsafePerformIO (
   newExternalEGen 0          >>= \ (egv, tev) ->
   writeVar primMouseTEv tev  >>
   return egv)

primMouseTEv :: MutVar (Time -> Event Point2)
primMouseTEv = unsafePerformIO (newVar (error "primMouseTEv"))

firePrimMouseEv :: Time -> Point2 -> IO ()
firePrimMouseEv t pt =
 fireExternalEGen t pt primMouseEGVar

primMousePos :: Time -> Event Point2
primMousePos = unsafePerformIO (readVar primMouseTEv)


primViewSzEGVar = 
 unsafePerformIO (
   newExternalEGen 0            >>= \ (egv, tev) ->
   writeVar primViewSzTEv tev   >>
   return egv)

{- Shortcut used by toplevel ev.loop -}
firePrimViewSize :: Time -> Int -> Int -> IO ()
firePrimViewSize t w h =
 fireExternalEGen t (vector2XY (fromInt w) (fromInt h)) primViewSzEGVar

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
firePrimFPS t fps =
 fireExternalEGen t fps primFPSEGVar

primFPSTEv :: MutVar (Time -> Event Double)
primFPSTEv = unsafePerformIO (newVar (error "primFPSTEv"))

primFPS :: Time -> Event Double
primFPS = unsafePerformIO (readVar primFPSTEv)

