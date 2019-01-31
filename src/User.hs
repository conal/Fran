-- The "User" type.  A user is simply a UserAction-valued event.

module User ( UserAction(..), User
            --, UserChannel, newUser, addUserUpdate
            , userStartTime, startTime
            ) where

import BaseTypes (Time)
import Event
import Vector2 (Vector2)
import Point2 (Point2)
import Win32 (VKey)
import Trace

-- A "user" is just a UserAction-valued event.
type User = Event UserAction

data UserAction
  = Resize	-- new size
      Vector2
  | Button
      Bool	-- left(True) or right(False)?
      Bool	-- down(True) or up(False)?  (dbl click == down)
      Point2
  | MouseMove
      Point2
  | Key
      Bool	-- press(True) or release(False) ?
      VKey	-- what key (its ASCII code - a first approx.)
  | UpdateDone
      Time	-- duration
  | Quit
  deriving Show

{-
-- So UserAction can derive Show.  Should be in Win32.hs, and more
-- informative.
instance Show VKey where
  showsPrec p _ = showString "<<VKey>>"
-}


-- Start time of user.
-- ## Bug: violates event abstraction.  Events don't really have start
-- times.  I put it in for convenience, but reconsider.

userStartTime :: User -> Time
userStartTime (Event ((t0,_) : _))  = t0

startTime = userStartTime               -- for compat