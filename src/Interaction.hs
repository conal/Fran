-- Higher-level interaction
--
-- Last modified Wed Aug 06 15:48:47 1997

module Interaction where

import BaseTypes
import Behavior
import qualified StaticTypes as S
import Vector2B
--import qualified VSBehavior as VSB
import Point2B
import Event
import User
import Trace
import Win32 (VKey)
import Monad (when)


-- Time relative to user

userTime :: User -> TimeB
userTime user = timeSince (startTime user)

filterButton :: Bool -> Bool  ->  User -> Event ()

filterButton wantLeft wantDown u = u `suchThat` f -=> ()
 where
  f (Button isLeft isDown _) = wantLeft == isLeft && wantDown == isDown
  f _			     = False


lbp, rbp, lbr, rbr :: User -> Event ()

lbp = filterButton True  True
rbp = filterButton False True
lbr = filterButton True  False
rbr = filterButton False False


filterKey :: Bool -> User -> Event VKey

filterKey wantDown u = u `filterE` f
 where
  f (Key isDown key)
    | isDown == wantDown = Just key
    | otherwise		 = Nothing
  f _			 = Nothing


keyPressAny, keyReleaseAny :: User -> Event VKey

keyPressAny   = filterKey True
keyReleaseAny = filterKey False

keyPress, keyRelease :: VKey -> User -> Event ()

keyPress   wantKey u = keyPressAny u   `suchThat` (== wantKey) -=> ()
keyRelease wantKey u = keyReleaseAny u `suchThat` (== wantKey) -=> ()


resize :: User -> Event S.Vector2
resize u = u `filterE` f
 where
  f (Resize size) = Just size
  f _		  = Nothing

mouseMove :: User -> Event S.Point2
mouseMove u = u `filterE` f
 where
  f (MouseMove pos) = --trace ("MouseMove " ++ show pos ++ "\n")$
		      Just pos
  f _		    = --trace "not mouse move\n"$
		      Nothing

updateDone :: User -> Event Time
updateDone u = u `filterE` f
 where
  f (UpdateDone dur) = Just dur
  f _		     = Nothing


userTimeIs :: Time -> User -> Event ()

userTimeIs dt u = timeIs (startTime u + dt)


-- Piecewise-constant input behaviors.

mouse        :: User -> Point2B
viewSize     :: User -> Vector2B
updatePeriod :: User -> Behavior Double

-- Define as piecewise constant.  The initial values are bogus.

mouse        = stepper S.origin2         . mouseMove
viewSize     = stepper (S.vector2XY 2 2) . resize
updatePeriod = stepper 0.1               . updateDone

-- utility
uStepper :: a -> (User -> Event a) -> (User -> Behavior a)
uStepper a0 uEvent u = stepper a0 (uEvent u)

