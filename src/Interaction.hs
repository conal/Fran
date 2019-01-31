-- Higher-level interaction
--
-- Last modified Mon Jul 14 17:56:39 1997

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

{-
userTimeIs te u = withTimeE u `filterE` f
 where
  f (_, t1)  |  t1 > uStart+te = Just ()
             |  otherwise      = Nothing
  uStart = startTime u
-}


-- Piecewise-constant input behaviors.  Problem: the initial values are
-- bogus.

mouse :: User -> Point2B
mouse u = loop (S.origin2, u)
 where
  loop :: (S.Point2, User) -> Point2B
  loop (p, u) =
   --trace ("mouse to " ++ show p ++ "\n") $
   constantB p `untilB` (mouseMove u `afterE` u) ==> loop

viewSize :: User -> Vector2B
viewSize u = loop (S.vector2XY 2 2, u)
 where
  loop (size, u) =
    -- Should be able to use `untilB` here, but I get
    -- Behavior Vector2 is not an instance of class "GBehavior".  Why?
    -- VSB.constantB size `VSB.UntilB` (resize u ==> loop)
    constantB size `untilB` (resize u `afterE` u) ==> loop

updatePeriod :: User -> Behavior Double
updatePeriod u = loop (0.1, u)
 where
  loop (dur, u) =
    --trace ("updatePeriod: " ++ show dur ++ " at " ++ show (startTime u) ++ "\n") $
    constantB dur `untilB` (updateDone u `afterE` u) ==> loop
